package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.InvalidEnumMappingException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.Individual;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.lang.reflect.Field;
import java.util.EnumMap;
import java.util.Map;

public class ObjectOneOfEnumConverter<E extends Enum<E>> implements ConverterWrapper<E, NamedResource> {

    private final EnumMap<E, NamedResource> valueMap;

    public ObjectOneOfEnumConverter(Class<E> enumType) {
        valueMap = buildValueMap(enumType);
    }

    private EnumMap<E, NamedResource> buildValueMap(Class<E> enumType) {
        final EnumMap<E, NamedResource> map = new EnumMap<>(enumType);
        try {
            for (Field f : enumType.getDeclaredFields()) {
                if (!f.isEnumConstant()) {
                    continue;
                }
                final Individual individual = f.getAnnotation(Individual.class);
                if (individual == null) {
                    throw new InvalidEnumMappingException(
                            "Enum constant " + f + " must be mapped to an ontological individual via the " + Individual.class.getSimpleName() + " annotation.");
                }
                map.put((E) f.get(null), NamedResource.create(individual.iri()));
            }
        } catch (IllegalAccessException e) {
            throw new OWLPersistenceException("Unable to initialize enum mapping.", e);
        }
        return map;
    }

    @Override
    public NamedResource convertToAxiomValue(E value) {
        assert value != null;
        return valueMap.get(value);
    }

    @Override
    public E convertToAttribute(NamedResource value) {
        assert value != null;
        return valueMap.entrySet().stream().filter(e -> e.getValue().equals(value)).findAny().map(Map.Entry::getKey)
                       .orElseThrow(
                               () -> new InvalidEnumMappingException("No enum constant mapped for value " + value));
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return NamedResource.class.isAssignableFrom(type);
    }
}
