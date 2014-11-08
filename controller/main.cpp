#include "X11/Xlib.h"
#include "X11/Xutil.h"

#include <EGL/egl.h>
#include <GLES2/gl2.h>

#define PNG_DEBUG 3
#include <png.h>

#include <stdexcept>
#include <sstream>
#include <fstream>
#include <memory>
#include <vector>
#include <string.h>

class EGLException : public std::runtime_error
{
public:
  EGLException(EGLint eglerr, const char* cond, const char* file, int line)
  :   std::runtime_error(format(eglerr, cond, file, line)) { /* NOP */ }

  static void test(const char* cond, const char* file, int line, bool mustfail)
  {
    EGLint eglerr = eglGetError();
    if(mustfail || eglerr != EGL_SUCCESS) throw EGLException(eglerr, cond, file, line);
  }

private:
  static std::string format(EGLint eglerr, const char* cond, const char* file, int line)
  {
    std::stringstream os;
    os << file << ":" << line << " eglErr=" << eglerr << " @ " << cond;
    return os.str();
  }
};

#define EGL_SAFE(c) do { if(!(c)) { EGLException::test(#c, __FILE__, __LINE__, true); } } while(0)
#define EGL_SANITYCHECK do { EGLException::test("sanity check", __FILE__, __LINE__, false); } while(0)

class XWin
{
public:
  XWin()
  {
    // Initializes the display and screen
    m_x11Display = XOpenDisplay(nullptr);
    if (!m_x11Display)
      throw std::runtime_error("Unable to open X display");
    m_x11Screen = XDefaultScreen(m_x11Display);

    // Gets the window parameters
    Window sRootWindow = RootWindow(m_x11Display, m_x11Screen);
    int depth = DefaultDepth(m_x11Display, m_x11Screen);
    XMatchVisualInfo(m_x11Display, m_x11Screen, depth, TrueColor, &m_x11Visual);
    m_x11Colormap = XCreateColormap(m_x11Display, sRootWindow, m_x11Visual.visual, AllocNone);

    XSetWindowAttributes attrs;
    attrs.colormap = m_x11Colormap;
    attrs.event_mask = StructureNotifyMask | ExposureMask | ButtonPressMask | ButtonReleaseMask | KeyPressMask | KeyReleaseMask;

    // Creates the X11 window
    m_x11Window = XCreateWindow(m_x11Display, RootWindow(m_x11Display, m_x11Screen), 0, 0, 800, 480,
                  0, CopyFromParent, InputOutput, CopyFromParent,
                  CWBackPixel | CWBorderPixel | CWEventMask | CWColormap, &attrs);
    XMapWindow(m_x11Display, m_x11Window);
    XFlush(m_x11Display);
  }

  ~XWin()
  {
    if(m_x11Window) XDestroyWindow(m_x11Display, m_x11Window);
    if(m_x11Colormap) XFreeColormap(m_x11Display, m_x11Colormap);
    if(m_x11Display) XCloseDisplay(m_x11Display);
  }

  void handleMessages()
  {
    int nMessages = XPending(m_x11Display);
    for(int i = 0; i < nMessages; ++ i)
    {
      XEvent event;
      XNextEvent(m_x11Display, &event);

      switch(event.type)
      {
      default:
        break;
      }
    }
  }

  Window getWindow() { return m_x11Window; }
  Display* getDisplay() { return m_x11Display; }

private:
  Window m_x11Window = 0;
  Display* m_x11Display = nullptr;
  long m_x11Screen = 0;
  XVisualInfo m_x11Visual;
  Colormap m_x11Colormap = 0;
};

class EGL
{
public:
  EGL()
  {
    m_eglDisplay = eglGetDisplay((EGLNativeDisplayType)m_xwin.getDisplay());

    if (!eglInitialize(m_eglDisplay, nullptr, nullptr))
    {
      throw std::runtime_error("Error: eglInitialize() failed.");
    }

    eglBindAPI(EGL_OPENGL_ES_API);
    EGL_SANITYCHECK;

    // choose config
    {
      static const EGLint attribs[] = {EGL_SURFACE_TYPE, EGL_WINDOW_BIT, EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT, EGL_NONE};

      EGLint config;
      if (!eglChooseConfig(m_eglDisplay, attribs, &m_eglConfig, 1, &config) || (config != 1))
      {
        throw std::runtime_error("Error: eglChooseConfig() failed.\n");
      }
    }

    m_eglSurface = eglCreateWindowSurface(m_eglDisplay, m_eglConfig, (EGLNativeWindowType)m_xwin.getWindow(), nullptr);
    EGL_SANITYCHECK;

    {
      static const EGLint attribs[] = {EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE};
      m_eglContext = eglCreateContext(m_eglDisplay, m_eglConfig, nullptr, attribs);
      EGL_SANITYCHECK;
    }

    eglMakeCurrent(m_eglDisplay, m_eglSurface, m_eglSurface, m_eglContext);
    EGL_SANITYCHECK;
  }

  ~EGL()
  {
    eglMakeCurrent(m_eglDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT) ;
    eglTerminate(m_eglDisplay);
  }

  void swapBuffers()
  {
    eglSwapBuffers(m_eglDisplay, m_eglSurface);
    EGL_SANITYCHECK;
  }

  XWin& getXWin() { return m_xwin; }

private:
  XWin m_xwin;

  EGLDisplay m_eglDisplay = 0;
  EGLConfig m_eglConfig = 0;
  EGLSurface m_eglSurface = 0;
  EGLContext m_eglContext = 0;
};

class GLSLShader
{
public:
  GLSLShader(std::string src, GLenum shaderType)
    : m_src(std::move(src)),
      m_shaderType(shaderType)
  {
    m_shader = glCreateShader(m_shaderType);

    const GLchar* psrc = m_src.data();
    GLint length = m_src.length();
    glShaderSource(m_shader, 1, &psrc, &length);
    glCompileShader(m_shader);

    GLint shaderCompiled;
    glGetShaderiv(m_shader, GL_COMPILE_STATUS, &shaderCompiled);
    if(! shaderCompiled)
    {
      int msgBufLength;
      glGetShaderiv(m_shader, GL_INFO_LOG_LENGTH, &msgBufLength);

      std::unique_ptr<char[]> msg(new char[msgBufLength]);
      int msgLen;
      glGetShaderInfoLog(m_shader, msgBufLength, &msgLen, msg.get());

      std::stringstream err;
      err << "shader compilation failed: " << msg.get();

      throw std::runtime_error(err.str());
    }
  }

  ~GLSLShader()
  {
    glDeleteShader(m_shader);
  }

  GLenum getShaderType() { return m_shaderType; }
  GLuint getShader() { return m_shader; }

private:
  std::string m_src;
  GLenum m_shaderType;

  GLuint m_shader;
};

class GLSLProgram
{
public:
  GLSLProgram(
    std::shared_ptr<GLSLShader> fragmentShader,
    std::shared_ptr<GLSLShader> vertexShader,
    const std::vector<std::string>& attribs)
  : m_fragmentShader(fragmentShader),
    m_vertexShader(vertexShader)
  {
    m_program = glCreateProgram();
    glAttachShader(m_program, m_fragmentShader->getShader());
    glAttachShader(m_program, m_vertexShader->getShader());

    {
      GLuint i;
      for(const auto& attrib: attribs)
        glBindAttribLocation(m_program, i++, attrib.c_str());
    }

    glLinkProgram(m_program);
    GLint bLinked;
    glGetProgramiv(m_program, GL_LINK_STATUS, &bLinked);
    if(! bLinked)
    {
      int msgBufLength;
      glGetProgramiv(m_program, GL_INFO_LOG_LENGTH, &msgBufLength);

      std::unique_ptr<char[]> msg(new char[msgBufLength]);
      int msgLen;
      glGetProgramInfoLog(m_program, msgBufLength, &msgLen, msg.get());

      std::stringstream err;
      err << "program linkage failed: " << msg.get();

      throw std::runtime_error(err.str());
    }
  }

  GLSLProgram(
    std::string fragmentShaderSrc,
    std::string vertexShaderSrc,
    const std::vector<std::string>& attribs)
  : GLSLProgram(
      std::make_shared<GLSLShader>(std::move(fragmentShaderSrc), GL_FRAGMENT_SHADER),
      std::make_shared<GLSLShader>(std::move(vertexShaderSrc), GL_VERTEX_SHADER),
      attribs)
  { /* NOP */ }

  ~GLSLProgram()
  {
    glDeleteProgram(m_program);
  }

  void use()
  {
    glUseProgram(m_program);
  }

  int getUniformLocation(const char* name)
  {
    return glGetUniformLocation(m_program, name);
  }

  GLuint getProgram() { return m_program; }

private:
  std::shared_ptr<GLSLShader> m_fragmentShader;
  std::shared_ptr<GLSLShader> m_vertexShader;

  GLuint m_program;
};

class GLBuffer
{
public:
  GLBuffer(size_t nElem, const GLfloat* data)
    : m_nElem(nElem)
  {
    glGenBuffers(1, &m_buffer);
    bind();
    glBufferData(GL_ARRAY_BUFFER, nElem * sizeof(GLfloat), data, GL_STATIC_DRAW);
  }

  ~GLBuffer()
  {
    glDeleteBuffers(1, &m_buffer);
  }

  void bind() { glBindBuffer(GL_ARRAY_BUFFER, m_buffer); }

private:
  size_t m_nElem;

  GLuint m_buffer;
};

class PNGLoader
{
public:
  PNGLoader(const std::string& filename)
  {
    png_image png;
    memset(&png, 0, sizeof(png_image));
    png.version = PNG_IMAGE_VERSION;

    if(! png_image_begin_read_from_file(&png, filename.c_str()))
      throw std::runtime_error("png_image_begin_read_from_file failed");

    png.format = PNG_FORMAT_RGBA;
    m_buffer.reset(new char[PNG_IMAGE_SIZE(png)]);

    m_width = png.width;
    m_height = png.height;

    if(! png_image_finish_read(&png, /*bg=*/nullptr, m_buffer.get(), /*row_stride=*/0, /*colormap=*/nullptr))
      throw std::runtime_error("png_image_finish_read failed");
  }

  char* getBuffer() const { return m_buffer.get(); }
  size_t getWidth() const { return m_width; }
  size_t getHeight() const { return m_height; }

private:
  std::unique_ptr<char[]> m_buffer;
  size_t m_width = 0;
  size_t m_height = 0;
};

class PNGTexture
{
public:
  PNGTexture(const std::string& filename)
  : m_loader(filename)
  {
    glGenTextures(1, &m_texture);
    bind();

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    glTexImage2D(GL_TEXTURE_2D, /*level=*/0, GL_RGBA, m_loader.getWidth(), m_loader.getHeight(), 0, GL_RGBA, GL_UNSIGNED_BYTE, m_loader.getBuffer());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  }

  ~PNGTexture()
  {
    glDeleteTextures(1, &m_texture);
  }

  void bind() { glBindTexture(GL_TEXTURE_2D, m_texture); }

private:
  PNGLoader m_loader;
  GLuint m_texture;
};

std::string readfile(const std::string& filename)
{
  std::ifstream in(filename);
  return std::string(
      std::istreambuf_iterator<char>(in),
      std::istreambuf_iterator<char>());
}

class Model
{
public:
  int getNumChannels() { return 2; }

private:

}

class View
{
public:
  View(Model* model)
  : m_model(model)
  {
  }

  void update()
  {
         
  }
  
  void draw()
  {
    
  }

private:
  Model* m_model; 
};

int main(int argc, char **argv)
{
  Model model;

  EGL egl;

  View view(&model);

  GLSLProgram program(readfile("ui.frag"), readfile("ui.vert"), {"vertex", "st"});
  program.use();

  PNGTexture texture("baboon.png");

  glClearColor(0.6f, 0.8f, 1.0f, 1.0f);

  glEnable(GL_CULL_FACE);

  GLfloat vertices[] = {
    -0.4f,-0.4f,0.0f,
    0.4f ,-0.4f,0.0f,
    0.0f ,0.4f ,0.0f};
  GLBuffer vertexBuffer(9, vertices);
  GLfloat sts[] = {0, 0, 1, 0, 1, 1};
  GLBuffer stBuffer(6, sts);

  for(int i = 0; i < 800; ++i)
  {
    glClear(GL_COLOR_BUFFER_BIT);

    glEnableVertexAttribArray(0);
    vertexBuffer.bind();
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glEnableVertexAttribArray(1);
    stBuffer.bind();
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, 0);

    glDrawArrays(GL_TRIANGLES, 0, 3);

    egl.swapBuffers();
    egl.getXWin().handleMessages();
  }

  return 0;
}
