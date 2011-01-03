package javax.microedition.io;

/**
 * @since MIDP 2.0
 */
public interface CommConnection extends StreamConnection {
    int getBaudRate();
    
    int setBaudRate(int baudrate);
}
