/*
uniform sampler2D colorMap;
uniform sampler2D envMap;
*/
varying vec3 N;
varying vec3 E;
uniform sampler2D envMap;

void main (void)
{
	/*
	vec4 color = texture2D( colorMap, gl_TexCoord[0].st);
	vec4 env = texture2D( envMap, gl_TexCoord[1].st);

	gl_FragColor = color + env*0.4;
	*/
	vec3 R = normalize(reflect(E,N));
	float x_winkel = (3.14159265 + atan(R.y,R.x))/(2.0*3.14159265);
	float z_winkel = atan(sqrt(R.x*R.x + R.y*R.y),R.z)/3.14159265;
    vec2 texCoords = vec2(x_winkel,z_winkel);
    vec4 color = texture2D(envMap,texCoords);
   	gl_FragColor = color;






}