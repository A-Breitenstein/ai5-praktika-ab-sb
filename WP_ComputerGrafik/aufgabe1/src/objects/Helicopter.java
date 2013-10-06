package objects;

import com.sun.j3d.utils.geometry.Box;
import com.sun.j3d.utils.geometry.Sphere;
import edu.cg1.exercises.introduction.AppearanceHelper;
import utils.CGkursUtils;

import javax.media.j3d.*;
import javax.vecmath.*;
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

    float rotorAngle = 0;
    float heckRotorAngle = 0;

    float y_rotation = 10;
    float speed = 15f;
    TransformGroup world;
    TransformGroup helicopter;
    TransformGroup heckRotor;
    TransformGroup rotor;





    private Helicopter(Vector3f pos, Color spotColor) {
        this.pos = pos;

        Transform3D t = new Transform3D();
        t.setTranslation(pos);
        world = new TransformGroup(t);
        TransformGroup tmpGroup;


        helicopter = new TransformGroup();
        Sphere cockpit = new Sphere(2f,AppearanceHelper.getAppearance(new Color(130, 178, 237), new Color(130, 178, 237)));
        helicopter.addChild(cockpit);


        Box kufe1 = new Box(3f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(60, 50, 47), new Color(60, 50, 47)));
        t.setTranslation(new Vector3f(0,-1.8f,-1f));
        tmpGroup = new TransformGroup(t);
        tmpGroup.addChild(kufe1);
        helicopter.addChild(tmpGroup);

        Box kufe2 = new Box(3f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(60, 50, 47), new Color(60, 50, 47)));
        t.setTranslation(new Vector3f(0,-1.8f,1f));
        tmpGroup = new TransformGroup(t);
        tmpGroup.addChild(kufe2);
        helicopter.addChild(tmpGroup);

        Box heck = new Box(4f, 0.3f, 0.3f, AppearanceHelper.getAppearance(new Color(30, 35, 65), new Color(30, 35, 65)));
        t.setTranslation(new Vector3f(4f,0.3f,0));
        tmpGroup = new TransformGroup(t);
        tmpGroup.addChild(heck);
        helicopter.addChild(tmpGroup);

        t.setTranslation(new Vector3f(7f,0.3f,-0.3f));
        heckRotor = new TransformGroup(t);

        Box heckRotor1 = new Box(0.2f, 1f, 0.2f, AppearanceHelper.getAppearance(new Color(232, 235, 237), new Color(237, 47, 47)));
        heckRotor.addChild(heckRotor1);

        Box heckRotor2 = new Box(1f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(232, 235, 237), new Color(237, 47, 47)));
        heckRotor.addChild(heckRotor2);

        helicopter.addChild(heckRotor);

        t.setTranslation(new Vector3f(0,2.05f,0));
        rotor = new TransformGroup(t);

        Box Rotor1 = new Box(4f, 0.2f, 0.2f, AppearanceHelper.getAppearance(new Color(18, 37, 38), new Color(18, 37, 38)));
        rotor.addChild(Rotor1);

        Box Rotor2 = new Box(0.2f, 0.2f, 4f, AppearanceHelper.getAppearance(new Color(18, 37, 38), new Color(18, 37, 38)));
        rotor.addChild(Rotor2);

        helicopter.addChild(rotor);
        world.addChild(helicopter);

        rotor.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        heckRotor.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        helicopter.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        world.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

        DirectionalLight light1 = new DirectionalLight();
        light1.setDirection(new Vector3f(0, -15, 0));
        light1.setColor(new Color3f(spotColor));
        light1.setInfluencingBounds(new BoundingSphere(new Point3d(-3, -10, 0.0), 12.0));
        helicopter.addChild(light1);


        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            long lastExecution = 0;
            @Override
            public void run() {
                lastExecution = System.currentTimeMillis();

                Transform3D transformation = new Transform3D();

                rotor.getTransform(transformation);

                //>>>>>>>>>>>>>>>>>>>>>>>>>>>CGkursUtils
                CGkursUtils.rotateY(rotorAngle, transformation);

                rotor.setTransform(transformation);
                rotorAngle+=2;
                if(rotorAngle > 360)
                    rotorAngle = 0;


                heckRotor.getTransform(transformation);

                //>>>>>>>>>>>>>>>>>>>>>>>>>>>CGkursUtils
                CGkursUtils.rotateZ(heckRotorAngle, transformation);

                heckRotor.setTransform(transformation);
                heckRotorAngle+=3;
                if(heckRotorAngle > 360)
                    heckRotorAngle = 0;

                helicopter.getTransform(transformation);
                double radian_angle = CGkursUtils.degreeToRadian(y_rotation *0.05);

                //>>>>>>>>>>>>>>>>>>>>>>>>>>>CGkursUtils
                CGkursUtils.rotateY(CGkursUtils.radianToDegree(radian_angle) - 90, transformation);

                helicopter.setTransform(transformation);

                world.getTransform(transformation);
                //>>>>>>>>>>>>>>>>>>>>>>>>>>>CGkursUtils
                CGkursUtils.rotateYMatrix4d(radian_angle, transformation);

                world.setTransform(transformation);
            }
        }, 10, 10);
        // Innere Klasse zum Verarbeiten der Timer-Ereignisse
    }

    public static Helicopter create(Vector3f pos, Color spotColor) {
        return new Helicopter(pos, spotColor);
    }

    public Group getGroup() {
        return world;
    }


}
