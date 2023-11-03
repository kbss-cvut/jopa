/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
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
