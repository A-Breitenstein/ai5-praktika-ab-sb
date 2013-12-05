package aufgabe7;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.TriangleMesh;

import java.io.*;
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
        return mesh;
    }

    private static void parse(String line, Map<String, List> structureMap) {

//        line.replace()
//        String[] elements = line.split(" ");
//        if (elements.length > 1) {
//            KeyWords.valueOf(elements[0])
//            switch ()
//
//        }
    }

    enum KeyWords{
        v,vt,vn,g,f;
    }
}
