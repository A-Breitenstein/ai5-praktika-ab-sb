package aufgabe7;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;
import aufgabe2.triangle.TriangleMesh;

import javax.vecmath.Point3d;
import javax.vecmath.TexCoord3f;
import javax.vecmath.Vector3f;
import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 05.12.13
 * Time: 11:42
 * To change this template use File | Settings | File Templates.
 */
public class ObjLoader {
    public static ITriangleMesh load(String path) {
        ITriangleMesh mesh = new TriangleMesh();
        Map<String, List> structureMap = new HashMap<String, List>();

        structureMap.put(KeyWords.v.toString(), new ArrayList<Point3d>());
        structureMap.put(KeyWords.vt.toString(), new ArrayList<TexCoord3f>());
        structureMap.put(KeyWords.vn.toString(), new ArrayList<Vector3f>());
        structureMap.put(KeyWords.g.toString(), new ArrayList<List>());
        structureMap.put(KeyWords.f.toString(), new ArrayList<Triangle>());

        try {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(path));
            while (bufferedReader.ready()) {
                parse(bufferedReader.readLine(),structureMap);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }


        for (Point3d point3d : (List<Point3d>) structureMap.get(KeyWords.v.name())) {
            mesh.addVertex(point3d);
        }
        for (TexCoord3f texCoord3f : (List<TexCoord3f>) structureMap.get(KeyWords.vt.name())) {
            mesh.addTexCoord(texCoord3f);
        }
        for (Vector3f vector3f : (List<Vector3f>) structureMap.get(KeyWords.vn.name())) {
            mesh.addVertexNormal(vector3f);
        }
        for (Triangle triangle : (List<Triangle>) structureMap.get(KeyWords.f.name())) {
            mesh.addTriangle(triangle);
        }


        return mesh;
    }

    private static void parse(String line, Map<String, List> structureMap) {
        if (line.trim().isEmpty()) {
            System.out.println("leeeeere zeile: " + line);
        }
        String[] elems = line.split(" ");
        if (elems[0].isEmpty() || elems[0].equals("#") || elems[0].equals(" ")) return;
        switch (KeyWords.valueOf(elems[0])) {
            case v:
                structureMap.get(KeyWords.v.name()).add(new Point3d(Double.valueOf(elems[1]), Double.valueOf(elems[2]), Double.valueOf(elems[3])));
                break;
            case vt:
                structureMap.get(KeyWords.vt.name()).add(new TexCoord3f(Double.valueOf(elems[1]).floatValue(), Double.valueOf(elems[2]).floatValue(), 0));
                break;
            case vn:
                structureMap.get(KeyWords.vn.name()).add(new Vector3f(Double.valueOf(elems[1]).floatValue(), Double.valueOf(elems[2]).floatValue(), Double.valueOf(elems[3]).floatValue()));
                break;
            case g:
//                throw new UnsupportedOperationException("g parameter nicht implementiert fail...");
                break;
            case f:
                if (elems.length > 4)
                    throw new IllegalArgumentException("Es lassen sich nur Dreiecksflächen definieren ( f parameter )");
                String[] vertices0 = elems[1].split("/");
                String[] vertices1 = elems[2].split("/");
                String[] vertices2 = elems[3].split("/");
                Triangle t;
                switch (vertices0.length) {
                    case 1:
                        t = new Triangle(Integer.valueOf(vertices0[0]).intValue() - 1, Integer.valueOf(vertices1[0]).intValue() - 1, Integer.valueOf(vertices2[0]).intValue() - 1);
                        structureMap.get(KeyWords.f.name()).add(t);
                        break;
                    case 2:
                        t = new Triangle(Integer.valueOf(vertices0[0]).intValue() - 1, Integer.valueOf(vertices1[0]).intValue() - 1, Integer.valueOf(vertices2[0]).intValue() - 1);
                        t.setTextureCoordinates(Integer.valueOf(vertices0[1]).intValue() - 1, Integer.valueOf(vertices1[1]).intValue() - 1, Integer.valueOf(vertices2[1]).intValue() - 1);
                        structureMap.get(KeyWords.f.name()).add(t);
                        break;
                    case 3:
                        t = new Triangle(Integer.valueOf(vertices0[0]).intValue() - 1, Integer.valueOf(vertices1[0]).intValue() - 1, Integer.valueOf(vertices2[0]).intValue() - 1);
                        t.setTextureCoordinates(Integer.valueOf(vertices0[1]).intValue() - 1, Integer.valueOf(vertices1[1]).intValue() - 1, Integer.valueOf(vertices2[1]).intValue() - 1);
                        t.setNormals(Integer.valueOf(vertices0[2]).intValue() - 1, Integer.valueOf(vertices1[2]).intValue() - 1, Integer.valueOf(vertices2[2]).intValue() - 1);
                        structureMap.get(KeyWords.f.name()).add(t);
                        break;
                    default:
                        break;
                }
                break;
        }
    }

    enum KeyWords{
        v,vt,vn,g,f;
    }

    public static void main(String[] args) {
        ObjLoader.load("Z:\\AI5\\computergrafikwp\\WP_ComputerGrafik\\aufgabe1\\src\\aufgabe7\\aufgabenblatt7_meshes\\bunny.obj");
    }
}
