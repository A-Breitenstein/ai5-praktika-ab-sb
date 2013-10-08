package aufgabe1.objects;

import com.sun.j3d.utils.geometry.Cylinder;
import com.sun.j3d.utils.geometry.Sphere;
import aufgabe1.grundgeruest.AppearanceHelper;

import javax.media.j3d.Group;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.vecmath.Vector3f;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 24.09.13
 * Time: 13:56
 * To change this template use File | Settings | File Templates.
 */
public class Tree {
    Vector3f pos;
    Sphere crown;
    Cylinder stem;
    Group group;

    private Tree(Vector3f pos, float crownRadius, float stemRadius, float stemHeight) {
        this.pos = pos;
        Transform3D t = new Transform3D();
        t.setTranslation(pos);
        group = new TransformGroup(t);


        // stem
        stem = new Cylinder(stemRadius, stemHeight);
        stem.setAppearance(AppearanceHelper.getAppearance(new Color(196, 135, 73),new Color(196, 135, 73)));
        t.setTranslation(new Vector3f(0, stemHeight * 0.5f, 0));
        TransformGroup crownYTransform = new TransformGroup(t);
        crownYTransform.addChild(stem);

        // crown
        crown = new Sphere(crownRadius);
        crown.setAppearance(AppearanceHelper.getAppearance(new Color(134, 196, 73),new Color(134, 196, 73)));
        t.setTranslation(new Vector3f(0, stemHeight, 0));
        TransformGroup cylinderYTransform = new TransformGroup(t);
        cylinderYTransform.addChild(crown);


        group.addChild(cylinderYTransform);
        group.addChild(crownYTransform);



    }

    public static Tree create(Vector3f pos, float crownRadius, float stemRadius, float stemHeight) {
        return new Tree(pos, crownRadius, stemRadius, stemHeight);
    }

    public Group getGroup() {
        return group;
    }

    public static Group plantTrees(int count,float x_size,float z_size) {
        Group trees = new Group();
        Map<String,Boolean> treeMap = new HashMap<String, Boolean>();
        Random x = new Random();
        Tree tree;
        String id;
        for (int i = 0; i < count; i++) {
            do{
            tree = create(new Vector3f(x.nextFloat()*x_size - (x_size*0.5f),0,x.nextFloat()*z_size - (z_size*0.5f)),x.nextFloat()*2+0.4f,x.nextFloat()*0.5f,x.nextFloat()*4+1.5f);
            id= (int)tree.pos.getX()+""+(int)tree.pos.getZ();
            }while(treeMap.containsKey(id));

            treeMap.put(id, true);
            trees.addChild(tree.getGroup());
        }
        return trees;
    }
}
