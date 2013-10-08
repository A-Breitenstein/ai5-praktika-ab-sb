package aufgabe1.objects;


import com.sun.j3d.utils.geometry.Box;
import aufgabe1.grundgeruest.AppearanceHelper;

import javax.media.j3d.Group;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.vecmath.Vector3f;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 24.09.13
 * Time: 14:50
 * To change this template use File | Settings | File Templates.
 */
public class Ground {
   Vector3f pos;
   Box plane;

    Group group;

    private Ground(Vector3f pos, float length, float width) {
        this.pos = pos;
        Transform3D t = new Transform3D();
        t.setTranslation(pos);
        group = new TransformGroup(t);
        plane = new Box(length,0,width, AppearanceHelper.getAppearance(new Color(26, 123, 36),new Color(248, 247, 255)));
        group.addChild(plane);

    }

    public static Ground create(Vector3f pos, float length, float width) {
        return new Ground(pos, length, width);
    }


    public Group getGroup() {
        return group;
    }
}
