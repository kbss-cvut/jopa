package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassN;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

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
        verify(validator).validateAnnotationPropertyField(OWLClassN.getAnnotationPropertyField());
    }
}