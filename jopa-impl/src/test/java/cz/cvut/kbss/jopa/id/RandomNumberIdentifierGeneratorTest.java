package cz.cvut.kbss.jopa.id;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RandomNumberIdentifierGeneratorTest {

    private static final URI CLASS_URI = URI.create("http://example.org/entities#TestEntity");

    @Mock
    private Metamodel metamodel;

    @Mock
    private Connection connection;

    private final RandomNumberIdentifierGenerator sut = new RandomNumberIdentifierGenerator();

    @BeforeEach
    void setUp() {
        reset(metamodel, connection);
    }

    private void configureMetamodel(URI classUri) {
        final EntityType<TestEntity> entityType = mock(EntityType.class);
        when(entityType.getIRI()).thenReturn(IRI.create(classUri.toString()));
        when(metamodel.entity(TestEntity.class)).thenReturn(entityType);
    }

    @Test
    void generateReturnsUniqueIdentifierWhenItDoesNotExist() throws Exception {
        configureMetamodel(CLASS_URI);
        when(connection.contains(any(Axiom.class), any())).thenReturn(false);

        final URI result = sut.generate(new TestEntity(), metamodel, connection);

        assertNotNull(result);
        assertTrue(result.toString().startsWith(CLASS_URI.toString()));
        final ArgumentCaptor<Axiom> captor = ArgumentCaptor.forClass(Axiom.class);
        verify(connection).contains(captor.capture(), any());
        assertEquals(CLASS_URI, captor.getValue().getValue().getValue());
        assertTrue(captor.getValue().getAssertion().isClassAssertion());
        assertFalse(captor.getValue().getAssertion().isInferred());
    }

    @Test
    void generateThrowsIdentifierGenerationExceptionWhenAllGeneratedIdentifiersExist() throws Exception {
        configureMetamodel(CLASS_URI);
        when(connection.contains(any(Axiom.class), any())).thenReturn(true);

        final IdentifierGenerationException ex = assertThrows(IdentifierGenerationException.class,
                () -> sut.generate(new TestEntity(), metamodel, connection));
        assertEquals("Unable to generate a unique identifier.", ex.getMessage());
        verify(connection, times(64)).contains(any(Axiom.class), any());
    }

    @Test
    void generateRetriesUntilItFindsUniqueIdentifier() throws Exception {
        configureMetamodel(CLASS_URI);
        when(connection.contains(any(Axiom.class), any())).thenReturn(true, true, false);

        final URI result = sut.generate(new TestEntity(), metamodel, connection);

        assertNotNull(result);
        verify(connection, times(3)).contains(any(Axiom.class), any());
    }

    @Test
    void generateThrowsIdentifierGenerationExceptionWhenConnectionFails() throws Exception {
        configureMetamodel(CLASS_URI);
        when(connection.contains(any(Axiom.class), any())).thenThrow(new OntoDriverException("Storage error"));

        final IdentifierGenerationException ex = assertThrows(IdentifierGenerationException.class,
                () -> sut.generate(new TestEntity(), metamodel, connection));
        assertTrue(ex.getMessage().contains("Unable to check if identifier"));
        assertEquals(OntoDriverException.class, ex.getCause().getClass());
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "http://example.org/Entity#Test",
            "http://example.org/Entity/",
            "http://example.org/Entity"
    })
    void generateBuildsUriAccordingToClassIriFormat(String classUriString) throws Exception {
        final URI classUri = URI.create(classUriString);
        configureMetamodel(classUri);
        when(connection.contains(any(Axiom.class), any())).thenReturn(false);

        final URI result = sut.generate(new TestEntity(), metamodel, connection);

        final String prefix = expectedPrefix(classUri);
        assertTrue(result.toString().matches(Pattern.quote(prefix) + "-?\\d+"));
    }

    private static String expectedPrefix(URI classUri) {
        if (classUri.getFragment() != null) {
            return classUri + "_instance";
        } else if (classUri.toString().endsWith("/")) {
            return classUri + "instance";
        } else {
            return classUri + "/instance";
        }
    }

    private static class TestEntity {
    }
}
