varying vec3 N;
varying vec3 E;
void main(){


       	// Normale vom Vertex berechnen
       	N = normalize(gl_NormalMatrix * gl_Normal);
    	// Richtung von der Kamera (Betrachter (E)) zum Vertex.
        vec3 v = vec3(gl_ModelViewMatrix * gl_Vertex);
    	// Richtung zur Kamera (Betrachter (E)) normalisiert
        E = normalize(-v);

        // Compute position in 3-space.
        gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

}