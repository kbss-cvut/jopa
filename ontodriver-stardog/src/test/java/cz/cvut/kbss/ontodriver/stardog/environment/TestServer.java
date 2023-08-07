package cz.cvut.kbss.ontodriver.stardog.environment;

import com.complexible.common.protocols.server.Server;
import com.complexible.common.protocols.server.ServerException;
import com.complexible.stardog.Stardog;
import com.complexible.stardog.api.admin.AdminConnection;
import com.complexible.stardog.api.admin.AdminConnectionConfiguration;
import cz.cvut.kbss.ontodriver.stardog.exception.StardogDriverException;

import java.net.InetSocketAddress;

public class TestServer {
    public static final String HOSTNAME = "localhost";
    public static final int PORT = 5820;
    public static final String DB_NAME = "test";

    private final Stardog stardog;
    private final Server server;

    public TestServer() throws StardogDriverException {
        // first need to initialize the Stardog instance
        this.stardog = Stardog.builder().create();
        // start a http server on the default port
        try {
            this.server = stardog.newServer()
                                 .bind(new InetSocketAddress(HOSTNAME, PORT))
                                 .start();
        } catch (ServerException e) {
            throw new StardogDriverException(e);
        }
        try (AdminConnection adminConnection = AdminConnectionConfiguration.toServer(getServerURL())
                                                                           .credentials("admin", "admin").connect()) {
            adminConnection.newDatabase(DB_NAME);
        }
    }

    public String getServerURL() {
        return "http://" + HOSTNAME + ":" + PORT;
    }

    public void shutdown() {
        try {
            // stop the http server
            server.stop();
        } finally {
            // stop the Stardog kernel
            stardog.shutdown();
        }
    }
}
