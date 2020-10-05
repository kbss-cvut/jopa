package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.spy;

class DataPropertyFieldStrategyTest {

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
    void getLanguagesUsesAttributeConfiguredLanguageWhenDescriptorDoesNotSpecifyAny() {
        final DataPropertyFieldStrategy<?, ?> sut = (DataPropertyFieldStrategy<?, ?>) FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                        metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertTrue(metamodelMocks.forOwlClassA().stringAttribute().hasLanguage());
        assertEquals(Generators.LANG, sut.getLanguage());
    }

    @Test
    void getLanguageRetrievesAttributeLanguageFromEntityDescriptor() {
        final String lang = "cs";
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), lang);
        final DataPropertyFieldStrategy<?, ?> sut = (DataPropertyFieldStrategy<?, ?>) FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                        metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertEquals(lang, sut.getLanguage());
    }

    @Test
    void getLanguageReturnsNullForSimpleLiteralAttribute() {
        final DataPropertyFieldStrategy<?, ?> sut = (DataPropertyFieldStrategy<?, ?>) FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassM().entityType(),
                        metamodelMocks.forOwlClassM().simpleLiteralAttribute(), descriptor, mapperMock);
        assertNull(sut.getLanguage());
    }
}
