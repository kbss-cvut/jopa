package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ChangeTrackingModeTest {

    @ParameterizedTest
    @MethodSource("configOptions")
    void resolveResolvesValueFromConfiguration(ChangeTrackingMode expected, String configValue) {
        final Configuration config = new Configuration(Map.of(JOPAPersistenceProperties.CHANGE_TRACKING_MODE, configValue));
        assertEquals(expected, ChangeTrackingMode.resolve(config));
    }

    static Stream<Arguments> configOptions() {
        return Stream.of(
                Arguments.of(ChangeTrackingMode.IMMEDIATE, "immediate"),
                Arguments.of(ChangeTrackingMode.IMMEDIATE, "IMMEDIATE"),
                Arguments.of(ChangeTrackingMode.ON_COMMIT, "on_commit"),
                Arguments.of(ChangeTrackingMode.ON_COMMIT, "ON_COMMIT")
        );
    }

    @Test
    void resolveReturnsOnCommitForRdf4jDataSourceIfValueIsNotExplicitlyConfigured() {
        final Configuration config = new Configuration(Map.of(JOPAPersistenceProperties.DATA_SOURCE_CLASS, "cz.cvut.kbss.ontodriver.rdf4j.Rdf4jDataSource"));
        assertEquals(ChangeTrackingMode.ON_COMMIT, ChangeTrackingMode.resolve(config));
    }

    @Test
    void resolveReturnsImmediateByDefault() {
        assertEquals(ChangeTrackingMode.IMMEDIATE, ChangeTrackingMode.resolve(new Configuration()));
    }
}
