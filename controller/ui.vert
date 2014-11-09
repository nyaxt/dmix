uniform highp mat4 mvp;
uniform highp vec2 texScale;
attribute highp vec2 vertex;
attribute highp vec2 st;
varying mediump vec2 texSt;

void main(void)
{
  gl_Position = mvp * vec4(vertex, 0.0, 1.0);
  texSt = st * texScale;
}
