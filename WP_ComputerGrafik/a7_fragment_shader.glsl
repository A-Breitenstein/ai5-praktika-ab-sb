
varying vec3 N;
varying vec3 E;
uniform sampler2D envMap;

void main (void)
{

    float pi = 3.14159265;
	vec3 R = normalize(reflect(E,N));
	float x_winkel = (pi + atan(R.y,R.x)) / (2.0*pi);
	float z_winkel = atan(sqrt(R.x*R.x + R.y*R.y),R.z) / pi;

    //TEST
	if(x_winkel > 0.5){x_winkel = 1;z_winkel = 1;}else{x_winkel = 0.1;z_winkel = 0.1;}

    vec2 texCoords = vec2(x_winkel,z_winkel);

    vec4 color = texture2D(envMap,texCoords);
   	gl_FragColor = color;






}