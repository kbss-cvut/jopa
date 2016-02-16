package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.EntityTypeImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import org.junit.Test;
import org.mockito.Mock;

import java.lang.reflect.Field;

public class EntityFieldMetamodelProcessorTest {

    @Mock
    private EntityTypeImpl<InvalidClass> etMock;
    @Mock
    private MetamodelImpl metamodelMock;

    @Test(expected = MetamodelInitializationException.class)
    public void processingNonTransientFieldWithoutPropertyInfoThrowsException() throws Exception {
        final EntityFieldMetamodelProcessor<InvalidClass> processor = new EntityFieldMetamodelProcessor<>(
                InvalidClass.class, etMock, metamodelMock);
        final Field field = InvalidClass.class.getDeclaredField("invalidAttribute");
        processor.processField(field);
    }

    private static final class InvalidClass {

        private String invalidAttribute;    // Attribute not transient but has no property/id info

        public String getInvalidAttribute() {
            return invalidAttribute;
        }

        public void setInvalidAttribute(String invalidAttribute) {
            this.invalidAttribute = invalidAttribute;
        }
    }

}