package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JFieldVar;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class JavaTransformerTest {

    private static final String ONTOLOGY_IRI = "http://onto.fel.cvut.cz/ontologies/owl2java/java-transformer-test";

    private OWLOntology ontology;

    private OWLDataFactory dataFactory;

    private JavaTransformer sut;

    @BeforeEach
    void setUp() throws Exception {
        final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        this.ontology = manager.createOntology(IRI.create(ONTOLOGY_IRI));
        this.dataFactory = new OWLDataFactoryImpl();
        this.sut = new JavaTransformer(TransformationConfiguration.builder().packageName("").build());
    }

    @Test
    void generatesValidJavaIdentifiersForIrisWithNonAsciiCharacters() {
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/slovník/agendový/popis-dat/pojem/navržený-term");
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        final ObjectModel result = sut.generateVocabulary(ontology, context);
        final JDefinedClass vocabClass = result.getCodeModel()._getClass(Constants.VOCABULARY_CLASS);
        assertNotNull(vocabClass);
        final Map<String, JFieldVar> fields = vocabClass.fields();
        assertTrue(fields.keySet().stream().anyMatch(n -> n.endsWith("navrzeny_term")));
    }
}