package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.Types;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class FieldMappingValidatorTest {

    private FieldMappingValidator validator = new FieldMappingValidator();

    @Test(expected = InvalidFieldMappingException.class)
    public void nonMapPropertiesFieldThrowsException() throws Exception {
        validator.validatePropertiesField(getField("values"));
    }

    private Field getField(String name) throws Exception {
        return InvalidClass.class.getDeclaredField(name);
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void rawPropertiesMapThrowsException() throws Exception {
        validator.validatePropertiesField(getField("rawProperties"));
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void propertiesFieldRequiresValueTypeToBeSet() throws Exception {
        validator.validatePropertiesField(getField("propertiesWithIntegerValue"));
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void propertiesFieldWithInvalidKeyTypeThrowsException() throws Exception {
        validator.validatePropertiesField(getField("propertiesWithInvalidKey"));
    }

    @Test
    public void validPropertiesFieldPassesValidation() throws Exception {
        validator.validatePropertiesField(getField("validProperties"));
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void nonSetTypesFieldThrowsException() throws Exception {
        validator.validateTypesField(getField("typesList"));
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void rawTypesSetThrowsException() throws Exception {
        validator.validateTypesField(getField("rawTypes"));
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void invalidTypesValueTypeThrowsException() throws Exception {
        validator.validateTypesField(getField("invalidValueTypes"));
    }

    @Test
    public void setOfUrisIsValidTypesField() throws Exception {
        validator.validateTypesField(getField("validTypes"));
    }

    private static final class InvalidClass {

        // Properties tests

        @Properties
        private Set<String> values;

        @Properties
        private Map rawProperties;

        @Properties
        private Map<String, Integer> propertiesWithIntegerValue;

        @Properties
        private Map<Long, Set<String>> propertiesWithInvalidKey;

        @Properties
        private Map<URI, Set<Object>> validProperties;

        // Types tests

        @Types
        private List<String> typesList;

        @Types
        private Set rawTypes;

        @Types
        private Set<Integer> invalidValueTypes;

        @Types
        private Set<URI> validTypes;
    }
}