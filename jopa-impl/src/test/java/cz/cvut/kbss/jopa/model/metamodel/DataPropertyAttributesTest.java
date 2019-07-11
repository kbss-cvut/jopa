package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.verify;

class DataPropertyAttributesTest {

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
    void resolveInvokesDataPropertyFieldValidation() throws Exception {
        final DataPropertyAttributes sut = new DataPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassA.getStrAttField(), metamodelBuilder,
                OWLClassA.getStrAttField().getType());
        verify(validator)
                .validateDataPropertyField(OWLClassA.getStrAttField(), OWLClassA.getStrAttField().getAnnotation(
                        OWLDataProperty.class));
    }

    @Test
    void resolveResolvesLexicalFormConfigurationFromAnnotation() throws Exception {
        final DataPropertyAttributes sut = new DataPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassM.getLexicalFormField(), metamodelBuilder, OWLClassM.getLexicalFormField().getType());
        assertTrue(sut.isLexicalForm());
    }

    @Test
    void resolveResolvesSimpleLiteralConfigurationFromAnnotation() throws Exception {
        final DataPropertyAttributes sut = new DataPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassM.getSimpleLiteralField(), metamodelBuilder, OWLClassM.getSimpleLiteralField().getType());
        assertTrue(sut.isSimpleLiteral());
    }
}