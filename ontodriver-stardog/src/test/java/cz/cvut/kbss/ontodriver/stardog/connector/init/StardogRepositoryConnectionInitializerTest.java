package cz.cvut.kbss.ontodriver.stardog.connector.init;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.stardog.environment.TestServer;
import org.eclipse.rdf4j.repository.Repository;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

// JNI bindings path for Stardog must be non-empty
@EnabledIfSystemProperty(named = "java.library.path", matches = "(.|\\s)*\\S(.|\\s)*")
// STARDOG_HOME must be non-empty (contains license)
@EnabledIfEnvironmentVariable(named = "STARDOG_HOME", matches = "(.|\\s)*\\S(.|\\s)*")
class StardogRepositoryConnectionInitializerTest {

    private static TestServer testServer;

    @BeforeAll
    static void setUpServer() throws Exception {
        testServer = new TestServer();
    }

    @AfterAll
    static void tearDownServer() {
        testServer.shutdown();
    }

    @Test
    void initializeRepositoryConnectsToStardogRepository() throws Exception {
        final DriverConfiguration config = new DriverConfiguration(OntologyStorageProperties.physicalUri(URI.create(testServer.getServerURL() + "/repositories/test"))
                                                                                            .build());
        final StardogRepositoryConnectionInitializer sut = new StardogRepositoryConnectionInitializer(config);
        sut.initializeRepository();
        final Repository repo = sut.getRepository();
        assertNotNull(repo);
        assertNull(sut.getManager());
        assertTrue(repo.isInitialized());
    }
}
