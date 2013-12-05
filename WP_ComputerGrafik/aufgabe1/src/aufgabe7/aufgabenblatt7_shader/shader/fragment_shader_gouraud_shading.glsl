varying vec4 color;

/** 
 * Fragment shader used for Gouraud shading. The vertex color is 
 * interpolated within the triangle.
 */ 
void main()
{
	// No operation required, just passing the color value from the vertex shader.
  	gl_FragColor = color;
}