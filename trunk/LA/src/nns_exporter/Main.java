package nns_exporter;

import java.io.*;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 04.11.13
 * Time: 12:53
 * To change this template use File | Settings | File Templates.
 */
public class Main {
    public static void main(String[] args) {
        List<CarState> carStates = readFileIn("8minTrack_steer_record.pat");
        int x = 1;
        for (CarState carState : carStates) {
            System.out.println(""+x++ +":" +carState);
        }
        normalize_to_1_network(carStates, "");
    }

    public static void normalize_to_1_network(List<CarState> liste, String outPutFilepath) {
        try {
            FileWriter file = new FileWriter(outPutFilepath+"10in3out_records.pat");
            file.write(createJavaNNSHeader(liste.size() - 1, 10, 3));

            StringBuilder sb;
            CarState currentState,nextState;
            int listSize = liste.size()-1;
            for (int i = 0; i < listSize; i++) {
                currentState = liste.get(i);
                nextState = liste.get(i+1);

                sb = new StringBuilder("");
                createInputNeurons_medium_precision(sb, currentState);
                file.write(sb.toString());

                file.write(decimalFormat("%.2f", nextState.accel) +" "+decimalFormat("%.2f", nextState.brake) +" "+decimalFormat("%.3f", nextState.steer) + "\r\n");

            }
            file.close();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }
    public static void normalize_to_3_networks(List<CarState> liste, String outPutFilepath){
        try {
            FileWriter steer_file = new FileWriter(outPutFilepath+"steer_records.pat");
            FileWriter accel_file = new FileWriter(outPutFilepath+"accel_records.pat");
            FileWriter brake_file = new FileWriter(outPutFilepath+"brake_records.pat");
            steer_file.write(createJavaNNSHeader(liste.size()-1,10,1));
            accel_file.write(createJavaNNSHeader(liste.size()-1,10,1));
            brake_file.write(createJavaNNSHeader(liste.size()-1,10,1));

            StringBuilder sb;
            CarState currentState,nextState;
            int listSize = liste.size()-1;
            for (int i = 0; i < listSize; i++) {
                currentState = liste.get(i);
                nextState = liste.get(i+1);

                // Anzahl der Möglichen Zustände
                sb = new StringBuilder("");
                createInputNeurons_medium_precision(sb, currentState);
                steer_file.write(sb.toString());
                accel_file.write(sb.toString());
                brake_file.write(sb.toString());

                steer_file.write(decimalFormat("%.3f", nextState.steer) + "\r\n");
                accel_file.write(decimalFormat("%.2f",nextState.accel)+"\r\n");
                brake_file.write(decimalFormat("%.2f",nextState.brake)+"\r\n");

            }
            steer_file.close();
            accel_file.close();
            brake_file.close();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    private static void createInputNeurons_low_precision(StringBuilder sb, CarState currentState) {
        sb.append(decimalFormat("%.1f",currentState.accel));   // 10
        sb.append(" ");
        sb.append(decimalFormat("%.1f",currentState.brake));   // 10
        sb.append(" ");
        sb.append(decimalFormat("%.3f",currentState.steer));  //1000
        sb.append(" ");
        sb.append(decimalFormat("%.2f",currentState.speed/10000.d));  //100
        sb.append(" ");

        sb.append(decimalFormat("%.1f",(double)currentState.distance_angled_sensorL2 / 100.d)); // 10
        sb.append(" ");
        sb.append(decimalFormat("%.1f",(double)currentState.distance_angled_sensorR2 / 100.d)); // 10
        sb.append(" ");

        sb.append(decimalFormat("%.1f",(double)currentState.distance_sensorLeft / 100.d)); // 10
        sb.append(" ");
        sb.append(decimalFormat("%.1f",(double)currentState.distance_sensorRight / 100.d)); // 10
        sb.append(" ");

        sb.append(decimalFormat("%.1f",(double)currentState.roadside_left_sensor / 100.d)); // 10
        sb.append(" ");
        sb.append(decimalFormat("%.1f",(double)currentState.roadside_right_sensor / 100.d)); // 10
        sb.append("\r\n");

    }
    private static void createInputNeurons_medium_precision(StringBuilder sb, CarState currentState) {
            sb.append(decimalFormat("%.2f",currentState.accel));   // 10
            sb.append(" ");
            sb.append(decimalFormat("%.2f",currentState.brake));   // 10
            sb.append(" ");
            sb.append(decimalFormat("%.3f",currentState.steer));  //1000
            sb.append(" ");
            sb.append(decimalFormat("%.2f",currentState.speed/10000.d));  //100
            sb.append(" ");

            sb.append(decimalFormat("%.2f",(double)currentState.distance_angled_sensorL2 / 100.d)); // 10
            sb.append(" ");
            sb.append(decimalFormat("%.2f",(double)currentState.distance_angled_sensorR2 / 100.d)); // 10
            sb.append(" ");

            sb.append(decimalFormat("%.2f",(double)currentState.distance_sensorLeft / 100.d)); // 10
            sb.append(" ");
            sb.append(decimalFormat("%.2f",(double)currentState.distance_sensorRight / 100.d)); // 10
            sb.append(" ");

            sb.append(decimalFormat("%.2f",(double)currentState.roadside_left_sensor / 100.d)); // 10
            sb.append(" ");
            sb.append(decimalFormat("%.2f",(double)currentState.roadside_right_sensor / 100.d)); // 10
            sb.append("\r\n");
    }

    private static void createInputNeurons_high_precision(StringBuilder sb, CarState currentState) {
        sb.append(decimalFormat("%.3f",currentState.accel));   // 10
        sb.append(" ");
        sb.append(decimalFormat("%.3f",currentState.brake));   // 10
        sb.append(" ");
        sb.append(decimalFormat("%.4f",currentState.steer));  //1000
        sb.append(" ");
        sb.append(decimalFormat("%.4f",currentState.speed/10000.d));  //100
        sb.append(" ");

        sb.append(decimalFormat("%.3f",(double)currentState.distance_angled_sensorL2 / 100.d)); // 10
        sb.append(" ");
        sb.append(decimalFormat("%.3f",(double)currentState.distance_angled_sensorR2 / 100.d)); // 10
        sb.append(" ");

        sb.append(decimalFormat("%.3f",(double)currentState.distance_sensorLeft / 100.d)); // 10
        sb.append(" ");
        sb.append(decimalFormat("%.3f",(double)currentState.distance_sensorRight / 100.d)); // 10
        sb.append(" ");

        sb.append(decimalFormat("%.3f",(double)currentState.roadside_left_sensor / 100.d)); // 10
        sb.append(" ");
        sb.append(decimalFormat("%.3f",(double)currentState.roadside_right_sensor / 100.d)); // 10
        sb.append("\r\n");
    }

    private static String createJavaNNSHeader(int patter_count,int inputs,int outputs) {
        return  "SNNS pattern definition file V3.2\r\n" +
                "generated at Mon Apr 25 18:08:50 1994\r\n" +
                "\r\n" +
                "No. of patterns : "+patter_count+"\r\n" +
                "No. of input units : "+inputs+"\r\n" +
                "No. of output units : "+outputs+"\r\n" +
                "\r\n" +
                "\r\n";
    }
    /**
     *
     * @param format   "#.#####"
     * @param value    0.912385
     * @return         0.91238
     */
    public static String decimalFormat(String format,double value) {
        DecimalFormat df = new DecimalFormat(format);
        return String.format(Locale.ENGLISH,format, value);
    }
    public static List<CarState> readFileIn(String filename){
        List<CarState> carStates = new ArrayList<CarState>();
        FileInputStream fi = null;
        try {
            fi = new FileInputStream(filename);
        } catch (FileNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

        try {

            BufferedReader buffer=new BufferedReader(new InputStreamReader(fi));
            for (int i = 0; i < 10; i++) {
                buffer.readLine();
            }
            String line;
            String[] values;

            CarState state;
            while((line = buffer.readLine()) != null){
                values = line.split(" ");
                if(!values[0].contains("#")){
                    state = new CarState();
                    state.accel = Double.valueOf(values[0]);
                    state.brake = Double.valueOf(values[1]);
                    state.speed = Double.valueOf(values[2]);
                    state.steer = Double.valueOf(values[3]);
                    state.gear = Integer.valueOf(values[4]);
                    state.roadside_right_sensor = Integer.valueOf(values[5]);
                    state.roadside_left_sensor = Integer.valueOf(values[6]);
                    state.distance_angled_sensorL2 = Integer.valueOf(values[7]);
                    state.distance_angled_sensorR2 = Integer.valueOf(values[8]);
                    state.distance_sensorLeft = Integer.valueOf(values[9]);
                    state.distance_sensorRight = Integer.valueOf(values[10]);

                    carStates.add(state);
                    buffer.readLine();// #result weg lesen
                    buffer.readLine();// result selbst weg lesen
                }
            }
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return carStates;
    }
}


/*
*  PATTERN
*  		   << round(100,currentState->accel) << " "
		   << round(100,currentState->brake) << " "
		   << round(10,currentState->speed) << " "
		   << round(PRECISION,currentState->steer) << " "
						   << currentState->gear << " "
						   << currentState->car_roadside_distance_percentage_right << " "
						   << currentState->car_roadside_distance_percentage_left << " "
						   << currentState->distance_angled_sensorL2 << " "
		                   << currentState->distance_angled_sensorR2 << " "
		                   << currentState->distance_sensorLeft << " "
		                   << currentState->distance_sensorRight << endl;
* */