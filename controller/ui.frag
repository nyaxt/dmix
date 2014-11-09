#define mediump
uniform sampler2D texture;
varying mediump vec2 tex_st;

void main (void)
{
  vec4 color = texture2D(texture, tex_st);
  gl_FragColor = color;
}
