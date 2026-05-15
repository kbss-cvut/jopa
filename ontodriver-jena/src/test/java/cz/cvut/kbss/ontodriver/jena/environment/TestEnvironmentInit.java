package cz.cvut.kbss.ontodriver.jena.environment;

import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.sys.JenaSystem;
import org.junit.platform.launcher.LauncherSession;
import org.junit.platform.launcher.LauncherSessionListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestEnvironmentInit implements LauncherSessionListener {

    private static final Logger LOG = LoggerFactory.getLogger(TestEnvironmentInit.class);

    @Override
    public void launcherSessionOpened(LauncherSession session) {
        LOG.info("Initializing Jena environment");
        JenaSystem.init();
        ResourceFactory.createResource("urn:warmup");
    }
}
