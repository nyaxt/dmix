/* uniform sampler2D texture; */

vec4 highlight()
{
  float r = 1.0 - distance(gl_FragCoord.xy, vec2(600, 100)) / 1000;
  r = min(1.0, r);
  return vec4(1.0, 1.0, 1.0, r);
}

void main(void)
{
  gl_FragColor = /*vec4(1.0, 0.2, 0.3, 0.5) +*/ highlight();
}
