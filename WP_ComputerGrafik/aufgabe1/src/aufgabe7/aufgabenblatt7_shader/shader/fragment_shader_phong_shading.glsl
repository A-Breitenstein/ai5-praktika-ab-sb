// These vectors need to be provided by the vertex shader
// Normal direction
varying vec3 N;
// Viewer direction
varying vec3 v;

/**
 * Fragment shader used fror phong shading. The lighting color 
 * value is computed for each pixel here.
 */
void main (void)
{
    // Define material
    vec4 ambientMat = vec4(0,0,0,1);
	vec4 diffuseMat = vec4(0.25,0.75, 0.25,1);
	vec4 specMat = vec4(1,1,1,1);
	float specPow = 20.0;
    
    // Define required variables.
    vec4 diffuse;
    vec4 spec;
    vec4 ambient;
    
    // Evaluate the required vectors for the Phong lighting model.
    // Attention: Phong shading vs. Phong lighting
    // Only the first light source is considered.
    vec3 L = normalize(gl_LightSource[0].position.xyz - v);
    vec3 E = normalize(-v);
    vec3 R = normalize(reflect(-L,N));
    
    // Compute the color from the three basic components
   	ambient = ambientMat;
  	diffuse = clamp( diffuseMat * max(dot(N,L), 0.0)  , 0.0, 1.0 ) ;
   	spec = clamp ( specMat * pow(max(dot(R,E),0.0),0.3*specPow) , 0.0, 1.0 );
    gl_FragColor = ambient + diffuse + spec;
}