package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver_new.exception.IdentifierGenerationException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class IdentifierGeneratorTest {

    @Mock
    private OWLOntology ontologyMock;

    private IdentifierGenerator generator;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.generator = new IdentifierGenerator(ontologyMock);
    }

    @Test
    public void generatesUniqueIdentifier() {
        final URI baseUri = URI.create("http://baseUri/");
        when(ontologyMock.containsIndividualInSignature(any(IRI.class))).thenReturn(false);
        final Set<URI> results = new HashSet<>();
        int tries = 100;
        for (int i = 0; i < tries; i++) {
            results.add(generator.generateIdentifier(baseUri));
        }
        results.forEach(uri -> assertTrue(uri.toString().contains(baseUri.toString())));
        assertEquals(tries, results.size());    // All tries lead to unique URI
    }

    @Test(expected = IdentifierGenerationException.class)
    public void throwsExceptionWhenUnableToGenerateUriWithinThreshold() throws Exception {
        final URI baseUri = URI.create("http://baseUri/");
        final int threshold = extractThreshold();
        when(ontologyMock.containsIndividualInSignature(any(IRI.class))).thenReturn(true);
        try {
            generator.generateIdentifier(baseUri);
        } finally {
            verify(ontologyMock, times(threshold)).containsIndividualInSignature(any(IRI.class));
        }
    }

    private int extractThreshold() throws Exception {
        final Field thresholdField = IdentifierGenerator.class.getDeclaredField("GENERATION_THRESHOLD");
        thresholdField.setAccessible(true);
        return (int) thresholdField.get(null);
    }
}