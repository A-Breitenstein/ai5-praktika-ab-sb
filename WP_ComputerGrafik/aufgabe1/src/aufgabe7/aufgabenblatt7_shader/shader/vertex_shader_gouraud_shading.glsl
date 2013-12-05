varying vec4 color;
varying vec3 N;
varying vec3 v;

/**
 * Vertex shader used for Gouraud shading. Evaluate the lighting using 
 * the Phong model at the vertices only.
 */
void main()
{
 	// Define material properties.
	vec4 ambientMat = vec4(0,0,0,1);
	vec4 diffuseMat = vec4(0.25,0.75, 0.25,1);
	vec4 specMat = vec4(1,1,1,1);
	float specPow = 20.0;
	
	// Define required variables.
	vec4 diffuse;
	vec4 spec;
	vec4 ambient;

	// Compute viewing direction.
   	v = vec3(gl_ModelViewMatrix * gl_Vertex);
   	// Compute normal direction.
   	N = normalize(gl_NormalMatrix * gl_Normal);
   	// Compute position in 3-space.
   	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;  

	// Normalize vectors. 
   	vec3 L = normalize(gl_LightSource[0].position.xyz - v);
   	vec3 E = normalize(-v);
   	vec3 R = normalize(reflect(-L,N)); 

	// Assemble color from the three basis components.
	ambient = ambientMat;
   	diffuse = clamp( diffuseMat * max(dot(N,L), 0.0)  , 0.0, 1.0 ) ;
   	spec = clamp ( specMat * pow(max(dot(R,E),0.0),0.3*specPow) , 0.0, 1.0 );
   	color = ambient + diffuse + spec;
}