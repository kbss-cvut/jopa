package cz.cvut.kbss.ontodriver.config;

import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

public class ConfigurationTest {

    private Configuration configuration = new Configuration();

    @Test
    public void addConfigurationFiltersKnownConfigurationProperties() {
        final Map<String, String> props = new HashMap<>();
        props.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, "true");
        props.put(OntoDriverProperties.ONTOLOGY_LANGUAGE, "en");
        props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, "false");

        configuration.addConfiguration(props,
                Arrays.asList(ConfigParam.AUTO_COMMIT, ConfigParam.MODULE_EXTRACTION_SIGNATURE,
                        ConfigParam.USE_TRANSACTIONAL_ONTOLOGY));
        assertNotNull(configuration.getProperty(ConfigParam.AUTO_COMMIT));
        assertNotNull(configuration.getProperty(ConfigParam.USE_TRANSACTIONAL_ONTOLOGY));
        assertNull(configuration.getProperty(ConfigParam.ONTOLOGY_LANGUAGE));   // Not in the known properties
    }

    @Test
    public void getWithDefaultReturnsDefaultWhenConfigParamValueIsNotFound() {
        assertNull(configuration.getProperty(ConfigParam.ONTOLOGY_LANGUAGE));
        final String defaultLang = "en";
        assertEquals(defaultLang, configuration.getProperty(ConfigParam.ONTOLOGY_LANGUAGE, defaultLang));
    }

    @Test
    public void testGetPropertyAsBoolean() {
        configuration.setProperty(ConfigParam.AUTO_COMMIT, "true");
        final boolean res = configuration.is(ConfigParam.AUTO_COMMIT);
        assertTrue(res);
    }

    @Test
    public void getAsBooleanReturnsFalseForUnknownProperty() {
        final boolean res = configuration.is(ConfigParam.AUTO_COMMIT);
        assertFalse(res);
    }
}