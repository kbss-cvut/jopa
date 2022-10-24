package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.Converter;
import cz.cvut.kbss.jopa.model.metamodel.ConverterResolver;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Registers custom attribute converters.
 *
 * @see cz.cvut.kbss.jopa.model.AttributeConverter
 */
class ConverterLoader implements Consumer<Class<?>> {

    private final Map<Class<?>, ConverterWrapper<?, ?>> converters = new HashMap<>();

    @Override
    public void accept(Class<?> cls) {
        final Converter converterAnn = cls.getAnnotation(Converter.class);
        if (converterAnn == null || !converterAnn.autoApply()) {
            return;
        }
        final ConverterWrapper<?, ?> converterWrapper = ConverterResolver.createCustomConverter(cls);
        final Class<?> attributeType = ConverterResolver.resolveConverterAttributeType(cls);
        converters.put(attributeType, converterWrapper);
    }

    public Map<Class<?>, ConverterWrapper<?, ?>> getConverters() {
        return converters;
    }
}
