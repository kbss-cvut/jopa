package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class AnnotationPropertyAttributesTest {

    @Mock
    private FieldMappingValidator validator;

    @Mock
    private MetamodelBuilder metamodelBuilder;

    @Mock
    private TypeBuilderContext<?> typeBuilderContext;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        doAnswer(invocation -> invocation.getArguments()[0]).when(typeBuilderContext).resolveNamespace(anyString());
    }

    @Test
    void resolveInvokesAnnotationPropertyFieldValidation() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassN.getAnnotationPropertyField(), metamodelBuilder,
                OWLClassN.getAnnotationPropertyField().getType());
        verify(validator).validateAnnotationPropertyField(OWLClassN.getAnnotationPropertyField(),
                OWLClassN.getAnnotationPropertyField().getAnnotation(OWLAnnotationProperty.class));
    }

    @Test
    void resolveResolvesLexicalFormConfigurationFromAnnotation() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(WithLexicalForm.class.getDeclaredField("lexicalForm"), metamodelBuilder, String.class);
        assertTrue(sut.isLexicalForm());
    }

    @SuppressWarnings("unused")
    private static class WithLexicalForm {
        @OWLAnnotationProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
        private String lexicalForm;
    }
}