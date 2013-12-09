//varying vec2 texture_coordinate;
varying vec3 N;
varying vec3 E;
void main(){
     /*
    gl_Position = ftransform();

    gl_TexCoord[0] = gl_MultiTexCoord0;

    vec3 u = normalize(vec3(gl_ModelViewMatrix * gl_Vertex));
    vec3 n = normalize(gl_NormalMatrix * gl_Normal);
    vec3 r = reflect(u,n);

    float m = 2.0 * sqrt(r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0));

    gl_TexCoord[1].s = r.x/m + 0.5;
    gl_TexCoord[1].t = r.y/m + 0.5;

    */

       	// Normale vom Vertex berechnen
       	N = normalize(gl_NormalMatrix * gl_Normal);
    	// Richtung von der Kamera (Betrachter (E)) zum Vertex.
        vec3 v = vec3(gl_ModelViewMatrix * gl_Vertex);
    	// Richtung zur Kamera (Betrachter (E)) normalisiert
        E = normalize(-v);

       	// Compute position in 3-space.
       	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}