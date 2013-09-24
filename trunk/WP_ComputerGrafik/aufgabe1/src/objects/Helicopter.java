package objects;

import com.sun.j3d.utils.geometry.Box;
import com.sun.j3d.utils.geometry.Sphere;
import edu.cg1.exercises.introduction.AppearanceHelper;

import javax.media.j3d.Group;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.vecmath.Matrix3f;
import javax.vecmath.Matrix4f;
import javax.vecmath.Vector3f;
import java.awt.*;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 24.09.13
 * Time: 16:18
 * To change this template use File | Settings | File Templates.
 */
public class Helicopter {
    Vector3f pos;
    Vector3f orientation;
    Vector3f rotation;

    float rotorAngle = 0;
    float heckRotorAngle = 0;


    float y_rotation = 80;
    float speed = 15f;

    TransformGroup helicopter;
    TransformGroup heckRotor;
    TransformGroup rotor;




    private Helicopter(Vector3f pos) {
        this.pos = pos;

        Transform3D t = new Transform3D();
        t.setTranslation(pos);
        helicopter = new TransformGroup(t);
        TransformGroup tmpGroup;


        Sphere cockpit = new Sphere(2f,AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        helicopter.addChild(cockpit);


        Box kufe1 = new Box(3f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        t.setTranslation(new Vector3f(0,-1.8f,-1f));
        tmpGroup = new TransformGroup(t);
        tmpGroup.addChild(kufe1);
        helicopter.addChild(tmpGroup);

        Box kufe2 = new Box(3f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        t.setTranslation(new Vector3f(0,-1.8f,1f));
        tmpGroup = new TransformGroup(t);
        tmpGroup.addChild(kufe2);
        helicopter.addChild(tmpGroup);

        Box heck = new Box(4f, 0.3f, 0.3f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        t.setTranslation(new Vector3f(4f,0.3f,0));
        tmpGroup = new TransformGroup(t);
        tmpGroup.addChild(heck);
        helicopter.addChild(tmpGroup);

        t.setTranslation(new Vector3f(7f,0.3f,-0.3f));
        heckRotor = new TransformGroup(t);

        Box heckRotor1 = new Box(0.2f, 1f, 0.2f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        heckRotor.addChild(heckRotor1);

        Box heckRotor2 = new Box(1f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        heckRotor.addChild(heckRotor2);

        helicopter.addChild(heckRotor);

        t.setTranslation(new Vector3f(0,2.05f,0));
        rotor = new TransformGroup(t);

        Box Rotor1 = new Box(4f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        rotor.addChild(Rotor1);

        Box Rotor2 = new Box(0.2f, 0.2f, 4f, AppearanceHelper.getAppearance(new Color(71, 237, 36), new Color(71, 237, 36)));
        rotor.addChild(Rotor2);

        rotor.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        heckRotor.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        helicopter.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

        helicopter.addChild(rotor);




        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                Transform3D transformation = new Transform3D();

                rotor.getTransform(transformation);
                Vector3f pos = new Vector3f();
                transformation.get(pos);
                transformation.setTranslation(new Vector3f(0,0,0));
                transformation.rotY(rotorAngle*3.1415f / 180);
                transformation.setTranslation(pos);
                rotor.setTransform(transformation);
                rotorAngle+=2;
                if(rotorAngle > 360)
                    rotorAngle = 0;


                heckRotor.getTransform(transformation);
                pos = new Vector3f();
                transformation.get(pos);
                transformation.setTranslation(new Vector3f(0, 0, 0));
                transformation.rotZ(heckRotorAngle * 3.1415f / 180);
                transformation.setTranslation(pos);
                heckRotor.setTransform(transformation);
                heckRotorAngle+=3;
                if(heckRotorAngle > 360)
                    heckRotorAngle = 0;

                helicopter.getTransform(transformation);
                transformation.get(pos);
                transformation.setTranslation(new Vector3f(0, 0, 0));
                transformation.rotY(y_rotation * 3.1415f / 180);
                float x_diff = (float)Math.sin(y_rotation * 3.1415f / 180)*speed;
                float z_diff = (float)Math.cos(y_rotation * 3.1415f / 180)*speed;
                pos.setX(pos.getX()+x_diff);
                pos.setZ(pos.getZ() + z_diff);
                transformation.setTranslation(pos);
                helicopter.setTransform(transformation);
                y_rotation++;
                if(y_rotation > 360)
                    y_rotation = 0;
                Matrix3f m3 =  new Matrix3f();

            }
        }, 10, 10);
        // Innere Klasse zum Verarbeiten der Timer-Ereignisse


    }

    public static Helicopter create(Vector3f pos) {
        return new Helicopter(pos);
    }

    public Group getGroup() {
        return helicopter;
    }


}
