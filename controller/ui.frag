uniform sampler2D texture;
varying mediump vec2 texSt;

void main (void)
{
  vec4 color = texture2D(texture, texSt);
  gl_FragColor = color;
}
