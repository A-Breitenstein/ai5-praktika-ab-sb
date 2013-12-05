varying vec3 N;
varying vec3 v;

/**
 * Vertex shader used for Phong shading. No color computations are 
 * required here, information is passed to the fragment shader.
 */
void main(void)
{
    // Compute the viewing direction.
    v = vec3(gl_ModelViewMatrix * gl_Vertex);
    // Compute the normal direction.
    N = normalize(gl_NormalMatrix * gl_Normal);
    // Compute the position in 3-space.
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    
}