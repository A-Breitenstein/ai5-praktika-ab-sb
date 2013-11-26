package client.starter;

import java.rmi.registry.Registry;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 21:41
 * To change this template use File | Settings | File Templates.
 */
public class Config {
    public static final int REGISTRY_PORT = Registry.REGISTRY_PORT; // standard RegistryPort
    public static final String REGISTRY_HOST = "localhost"; //ist selbe adresse wie die des dispatchers
    public static final String MONITOR_NAME = "monitor";
    public static final String HAWMPS1_NAME = "hawmps1";
    public static final String HAWMPS2_NAME = "hawmps2";
}
