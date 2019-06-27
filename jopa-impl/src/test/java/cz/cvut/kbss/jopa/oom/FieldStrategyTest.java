package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

class FieldStrategyTest {

    @Mock
    private EntityMappingHelper mapperMock;


    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.metamodelMocks = new MetamodelMocks();
        descriptor = spy(descriptor);
    }

    @Test
    void getLanguageRetrievesAttributeLanguageFromEntityDescriptor() throws Exception {
        final String lang = "cs";
        descriptor.setAttributeLanguage(OWLClassA.getStrAttField(), lang);
        final FieldStrategy<?, ?> sut = FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                        metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertEquals(lang, sut.getLanguage());
    }

    @Test
    void getAttributeContextRetrievesContextFromEntityDescriptor() throws Exception {
        final URI context = Generators.createIndividualIdentifier();
        descriptor.addAttributeContext(OWLClassA.getStrAttField(), context);
        final FieldStrategy<?, ?> sut = FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                        metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertEquals(context, sut.getAttributeContext());
        verify(descriptor).getAttributeContext(metamodelMocks.forOwlClassA().stringAttribute());
    }
}