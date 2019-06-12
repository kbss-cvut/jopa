package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

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
    void resolveInvokesAnnotationPropertyFieldValidation() throws Exception {
        final DataPropertyAttributes sut = new DataPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassA.getStrAttField(), metamodelBuilder,
                OWLClassA.getStrAttField().getType());
        verify(validator).validateDataPropertyField(OWLClassA.getStrAttField());
    }

}