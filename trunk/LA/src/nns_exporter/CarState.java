package nns_exporter;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 04.11.13
 * Time: 13:08
 * To change this template use File | Settings | File Templates.
 */
public class CarState {
    /*
    * struct car_state{

	int gear,distance_sensorLeft, distance_sensorRight, distance_angled_sensorL2, distance_angled_sensorR2,
		car_roadside_distance_percentage_left,
		car_roadside_distance_percentage_right;
	float steer,accel,brake,speed;


};
    * */

    public CarState() {
    }

    public int gear, distance_sensorLeft,distance_sensorRight,distance_angled_sensorL2,distance_angled_sensorR2,
               roadside_left_sensor,roadside_right_sensor;
    public double steer,accel,brake,speed;

    @Override
    public String toString() {
        return "CarState{" +
                "gear=" + gear +
                ", distance_sensorLeft=" + distance_sensorLeft +
                ", distance_sensorRight=" + distance_sensorRight +
                ", distance_angled_sensorL2=" + distance_angled_sensorL2 +
                ", distance_angled_sensorR2=" + distance_angled_sensorR2 +
                ", roadside_left_sensor=" + roadside_left_sensor +
                ", roadside_right_sensor=" + roadside_right_sensor +
                ", steer=" + steer +
                ", accel=" + accel +
                ", brake=" + brake +
                ", speed=" + speed +
                '}';
    }
}
