varying vec2 texture_coordinate;
uniform sampler2D my_color_texture;

/**
 * Fragment shader used for texture mapping.
 */
void main()
{
    // Sampling the texture and passing it to the frame buffer
    gl_FragColor = texture2D(my_color_texture, texture_coordinate);
    
    // Basically we are done already. This code inserts a red and
    // blue cross inside the rendered primitive.
    if ( texture_coordinate.x > 0.4 && texture_coordinate.x < 0.6 )
    {
    	gl_FragColor = vec4(0.25, 0.25, 0.75, 1.0);
    } else if ( texture_coordinate.y > 0.4 && texture_coordinate.y < 0.6 )
    {
    	gl_FragColor = vec4(0.75, 0.25, 0.25, 1.0);
    } 
    
   
}