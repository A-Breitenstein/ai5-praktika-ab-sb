/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe6.grundgeruest;

import aufgabe6.Curve;
import aufgabe6.HermiteCurve;
import aufgabe6.MonomialCurve;
import aufgabe6.Plotter;
import com.sun.j3d.utils.behaviors.vp.OrbitBehavior;
import com.sun.j3d.utils.geometry.Box;
import com.sun.j3d.utils.geometry.Cylinder;
import com.sun.j3d.utils.geometry.Sphere;
import com.sun.j3d.utils.pickfast.behaviors.PickTranslateBehavior;
import com.sun.j3d.utils.universe.SimpleUniverse;

import javax.media.j3d.*;
import javax.swing.*;
import javax.vecmath.*;
import java.awt.*;

/**
 * Central widget for the 3D graphics exercises.
 * 
 * @author Philipp Jenke
 * 
 */
public class CG1Frame extends JFrame {

    /**
     * Required for Serializable interface.
     */
    private static final long serialVersionUID = -8406043101882693554L;

    /**
     * Canvas object for the 3D content.
     */
    public static Canvas3D canvas3D;

    /**
     * Simple universe (provides reasonable default values).
     */
    protected SimpleUniverse universe;

    /**
     * Scene graph for the 3D content scene.
     */
    protected BranchGroup scene = new BranchGroup();
    public static Regler  regler;
    public Shape3D tangentVector;
    public Curve curve;
    private BranchGroup branchGroup;

    /**
     * Default constructor.
     */
    public CG1Frame() {
        initRegler();
        // Create canvas object to draw on
        canvas3D = new Canvas3D(SimpleUniverse.getPreferredConfiguration());

        // The SimpleUniverse provides convenient default settings
        universe = new SimpleUniverse(canvas3D);
        universe.getViewingPlatform().setNominalViewingTransform();


        // Setup lighting
        addLight(universe);

        // Allow for mouse control
        OrbitBehavior ob = new OrbitBehavior(canvas3D);
        ob.setSchedulingBounds(new BoundingSphere(new Point3d(0, 0, 0),
                Double.MAX_VALUE));
        universe.getViewingPlatform().setViewPlatformBehavior(ob);

        // Set the background color
        Background background = new Background(new Color3f(0.9f, 0.9f, 0.9f));
        BoundingSphere sphere = new BoundingSphere(new Point3d(0, 0, 0), 100000);
        background.setApplicationBounds(sphere);
        scene.addChild(background);

        // Setup frame
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setTitle("Einführung in die Computergrafik");
        setSize(1000, 1000);
        getContentPane().add("Center", canvas3D);
        setVisible(true);
    }

    private void initRegler() {
        JFrame frame = new JFrame("Regler");
        regler = new Regler();
        regler.setCg1Frame(this);
        frame.setContentPane(regler.getPanel1());
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }

    /**
     * Setup the lights in the scene. Attention: The light need to be added to
     * the scene before the scene in compiled (see createSceneGraph()).
     */
    private void addLight(SimpleUniverse universe) {
//        addPointLight(new Point3f(1, 20, 1));
//        addPointLight(new Point3f(-1, 20, -1));
//        addPointLight(new Point3f(1, 20, 1));
//        addDirectionalLight(new Vector3f(0, 0, 1));

        PointLight light = new PointLight();
        light.setPosition(new Point3f(0.f,15.f,10.f));
        light.setEnable(true);
        light.setColor(new Color3f(new Color(238, 245, 245)));
        light.setCapability(AmbientLight.ALLOW_STATE_WRITE);
        light.setCapability(AmbientLight.ALLOW_COLOR_WRITE);
        light.setInfluencingBounds(new BoundingSphere(
                new Point3d(0.f,15.f,10.f), 1500.0));
        scene.addChild(light);

//        DirectionalLight light1 = new DirectionalLight();
//        light1.setDirection(new Vector3f(0,-10,0));
//        light1.setColor(new Color3f(new Color(251, 248, 52)));
//        light1.setInfluencingBounds(new BoundingSphere(
//                new Point3d(0.0, 0.0, 0.0), 12.0));
//        scene.addChild(light1);


    }

    void addPointLight(Point3f position) {
        PointLight light = new PointLight();
        light.setPosition(position);
        light.setColor(new Color3f(1, 1, 1));
        light.setInfluencingBounds(new BoundingSphere(
                new Point3d(0.0, 900, 0.0), 500.0));
        scene.addChild(light);
    }

    //TODO: parameter for color and Boundingsphere
    void addDirectionalLight(Vector3f direction) {
        DirectionalLight light = new DirectionalLight();
        light.setDirection(direction);
        light.setColor(new Color3f(1, 1, 1));
        light.setInfluencingBounds(new BoundingSphere(
                new Point3d(0.0, 0.0, 0.0), 100.0));
        scene.addChild(light);
    }

    /**
     * Create the default scene graph.
     */
    protected void createSceneGraph() {
        // Add a coordinate system to the scene.
        scene.addChild(createCoordinateSystem());

        Vector3d v1 = new Vector3d(.1,.2,.3);
        Vector3d v2 = new Vector3d(.4,.4,.9);
        Vector3d v3 = new Vector3d(-.2,.3,-.5);
        Vector3d v4 = new Vector3d(.35,0,0);
        Vector3d v5 = new Vector3d(.3,-.2,.4);
        Vector3d v6 = new Vector3d(.1,-.4,.2);

        MonomialCurve monocurve = MonomialCurve.create(6);

        monocurve.setControlPoint(0,v1);
        monocurve.setControlPoint(1,v2);
        monocurve.setControlPoint(2,v3);
        monocurve.setControlPoint(3,v4);
        monocurve.setControlPoint(4,v5);
        monocurve.setControlPoint(5,v6);

//        curve = monocurve;
//        curve = MonomialCurve.create(v1, v2, v3, v4);
        curve = HermiteCurve.create(v1, v2, v3, v4);

        scene.addChild(Plotter.plottFunction(curve,1000));

        BoundingSphere behaveBounds = new BoundingSphere();
        PickTranslateBehavior pickTranslate = new PickTranslateBehavior(scene,
                CG1Frame.canvas3D, behaveBounds);
        scene.addChild(pickTranslate);

        Vector3d source, target;

        source = curve.eval(0.d);
        target = curve.derivative(0.d);

        tangentVector = createTangentVector(source, target);
        branchGroup = new BranchGroup();
        branchGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        branchGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
        branchGroup.setCapability(BranchGroup.ALLOW_DETACH);

        branchGroup.addChild(tangentVector);


        scene.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        scene.setCapability(Group.ALLOW_CHILDREN_WRITE);
        scene.addChild(branchGroup);


        // Assemble scene
        scene.compile();

        universe.getViewer().getView().setSceneAntialiasingEnable(true);
        universe.getViewer().getView().setFieldOfView(1.5f);
        universe.getViewer().getView().setBackClipDistance(500);
        universe.addBranchGraph(scene);
    }

    private Shape3D createTangentVector(Vector3d source, Vector3d target) {
        Point3f[] dotPts = new Point3f[2];
        dotPts[0] = new Point3f(source);
        dotPts[1] = new Point3f(target);
        LineArray dot = new LineArray(2, LineArray.COORDINATES);
        dot.setCoordinates(0, dotPts);
        LineAttributes dotLa = new LineAttributes();
        dotLa.setLineWidth(20.0f);
        dotLa.setLinePattern(LineAttributes.PATTERN_SOLID);
        Appearance dotApp = new Appearance();
        ColoringAttributes ca = new ColoringAttributes(new Color3f(new Color(206, 0, 9)),
                ColoringAttributes.SHADE_FLAT);
        dotApp.setColoringAttributes(ca);
        dotApp.setLineAttributes(dotLa);
        dotApp.setColoringAttributes(ca);
        Shape3D dotShape = new Shape3D(dot, dotApp);
        return dotShape;
    }

    public void changeTangentVector(double position) {
        final double val = position;
        tangentVector = createTangentVector(curve.eval(val), curve.derivative(val));

        branchGroup.detach();
        branchGroup = new BranchGroup();
        branchGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        branchGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
        branchGroup.setCapability(BranchGroup.ALLOW_DETACH);

        branchGroup.addChild(tangentVector);

        scene.addChild(branchGroup);

    }


    protected Node createBox(){
        Box box = new Box();
        AppearanceHelper.setColor(box, new Color3f(0.75f, 0.25f, 0.25f));
        Transform3D scale = new Transform3D();
        scale.setScale(0.25);
        TransformGroup transformation = new TransformGroup(scale);
        transformation.addChild(box);
        return transformation;
    }
    /**
     * Create a group to represent the coordinate system.
     *
     * @return
     */
    protected Node createCoordinateSystem() {

        Group group = new Group();

        // X-coordinate axis - line
        Cylinder cylinderX = new Cylinder(0.02f, 1.0f);
        AppearanceHelper.setColor(cylinderX, new Color3f(1, 0, 0));
        Transform3D tX = new Transform3D();
        tX.setTranslation(new Vector3f(0, 0.5f, 0));
        TransformGroup cylinderXTransform = new TransformGroup(tX);
        cylinderXTransform.addChild(cylinderX);
        Transform3D tXRotate = new Transform3D();
        tXRotate.rotZ(-90.0 * Math.PI / 180.0);
        TransformGroup xAxis = new TransformGroup(tXRotate);
        xAxis.addChild(cylinderXTransform);
        group.addChild(xAxis);

        // X-coordinate axis - end point
        Sphere sphereX = new Sphere(0.03f);
        Transform3D tSphereX = new Transform3D();
        tSphereX.setTranslation(new Vector3f(1, 0, 0));
        TransformGroup tgSphereX = new TransformGroup(tSphereX);
        tgSphereX.addChild(sphereX);
        AppearanceHelper.setColor(sphereX, new Color3f(1, 0, 0));
        group.addChild(tgSphereX);

        // Y-coordinate axis - line
        Cylinder cylinderY = new Cylinder(0.02f, 1.0f);
        AppearanceHelper.setColor(cylinderY, new Color3f(0, 1, 0));
        Transform3D tY = new Transform3D();
        tY.setTranslation(new Vector3f(0, 0.5f, 0));
        TransformGroup cylinderYTransform = new TransformGroup(tY);
        cylinderYTransform.addChild(cylinderY);
        group.addChild(cylinderYTransform);

        // Y-coordinate axis - end point
        Sphere sphereY = new Sphere(0.03f);
        Transform3D tSphereY = new Transform3D();
        tSphereY.setTranslation(new Vector3f(0, 1, 0));
        TransformGroup tgSphereY = new TransformGroup(tSphereY);
        tgSphereY.addChild(sphereY);
        AppearanceHelper.setColor(sphereY, new Color3f(0, 1, 0));
        group.addChild(tgSphereY);

        // Z-coordinate axis - line
        Cylinder cylinderZ = new Cylinder(0.02f, 1.0f);
        AppearanceHelper.setColor(cylinderZ, new Color3f(0, 0, 1));
        Transform3D tZ = new Transform3D();
        tZ.setTranslation(new Vector3f(0, 0.5f, 0));
        TransformGroup cylinderZTransform = new TransformGroup(tZ);
        cylinderZTransform.addChild(cylinderZ);
        Transform3D tZRotate = new Transform3D();
        tZRotate.rotX(90.0 * Math.PI / 180.0);
        TransformGroup zAxis = new TransformGroup(tZRotate);
        zAxis.addChild(cylinderZTransform);
        group.addChild(zAxis);

        // Z-coordinate axis - end point
        Sphere sphereZ = new Sphere(0.03f);
        Transform3D tSphereZ = new Transform3D();
        tSphereZ.setTranslation(new Vector3f(0, 0, 1));
        TransformGroup tgSphereZ = new TransformGroup(tSphereZ);
        tgSphereZ.addChild(sphereZ);
        AppearanceHelper.setColor(sphereZ, new Color3f(0, 0, 1));
        group.addChild(tgSphereZ);

        return group;
    }

    /**
     * Program entry point.
     */
    public static void main(String[] args) {
        // Create the central frame
        CG1Frame frame = new CG1Frame();
        // Add content to the scene graph
        frame.createSceneGraph();
    }


}
