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

import cz.cvut.kbss.jopa.environment.ZoneOffsetConverter;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.exception.InstantiationException;
import cz.cvut.kbss.jopa.exception.InvalidConverterException;
import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.annotations.Converter;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.CustomConverterWrapper;
import org.junit.jupiter.api.Test;

import java.time.Period;
import java.time.ZoneOffset;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ConverterLoaderTest {

    private final ConverterLoader sut = new ConverterLoader();

    @Test
    void acceptDoesNothingWhenClassHasNoConverterAnnotation() {
        assertThat(sut.getConverters().keySet(), empty());
        sut.accept(this.getClass());
        assertThat(sut.getConverters().keySet(), empty());
    }

    @Test
    void acceptCreatesConverterWrapperForSpecifiedConverterClassInstanceWhenAutoApplyIsTrue() {
        sut.accept(AutoAppliedConverter.class);
        assertThat(sut.getConverters().keySet(), not(empty()));
        assertEquals(1, sut.getConverters().size());
        final ConverterWrapper<?, ?> wrapper = sut.getConverters().get(ZoneOffset.class);
        assertThat(wrapper, instanceOf(CustomConverterWrapper.class));
        assertThat(((CustomConverterWrapper<?, ?>) wrapper).getWrappedConverter(),
                   instanceOf(AutoAppliedConverter.class));
    }

    @TestLocal
    @Converter(autoApply = true)
    public static class AutoAppliedConverter implements AttributeConverter<ZoneOffset, String> {

        @Override
        public String convertToAxiomValue(ZoneOffset value) {
            return value.getId();
        }

        @Override
        public ZoneOffset convertToAttribute(String value) {
            return ZoneOffset.of(value);
        }
    }

    @Test
    void acceptDoesNothingWhenConverterAutoApplyIsFalse() {
        assertThat(sut.getConverters().keySet(), empty());
        sut.accept(ZoneOffsetConverter.class);
        assertThat(sut.getConverters().keySet(), empty());
    }

    @Test
    void acceptThrowsInvalidConverterExceptionWhenAnnotatedClassDoesNotImplementAttributeConverter() {
        final InvalidConverterException e =
                assertThrows(InvalidConverterException.class, () -> sut.accept(InvalidTypeConverter.class));
        assertThat(e.getMessage(), containsString(AttributeConverter.class.getSimpleName()));
    }

    @TestLocal
    @Converter(autoApply = true)
    static class InvalidTypeConverter {
    }

    @Test
    void acceptThrowsInvalidConverterExceptionWhenAnnotatedClassDoesNotHavePublicNoArgConstructor() {
        final InvalidConverterException e =
                assertThrows(InvalidConverterException.class, () -> sut.accept(MissingNoArgConstructorConverter.class));
        assertThat(e.getCause(), instanceOf(InstantiationException.class));
    }

    @TestLocal
    @Converter(autoApply = true)
    static class MissingNoArgConstructorConverter implements AttributeConverter<Period, String> {

        public MissingNoArgConstructorConverter(String argument) {
            // Do nothing
        }

        @Override
        public String convertToAxiomValue(Period value) {
            return null;
        }

        @Override
        public Period convertToAttribute(String value) {
            return null;
        }
    }
}