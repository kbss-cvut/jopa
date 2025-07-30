/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.net.URL;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Period;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Datatype specifications in the assertions are taken from the SPARQL specification.
 */
@ExtendWith(MockitoExtension.class)
class ParameterValueFactoryTest {

    @Mock
    private MetamodelProvider metamodelProvider;

    @InjectMocks
    private ParameterValueFactory sut;

    @Test
    void createStringValueWithoutLanguageTag() {
        final ParameterValue value = sut.create("test");
        assertEquals("test", value.getValue());
        assertEquals("\"test\"", value.getQueryString());
    }

    @Test
    void createStringValueWithLanguageTag() {
        final ParameterValue value = sut.create("test", "en");
        assertEquals("test", value.getValue());
        assertEquals("\"test\"@en", value.getQueryString());
    }

    @Test
    void createBooleanValue() {
        final ParameterValue value = sut.create(true);
        assertEquals(Boolean.TRUE, value.getValue());
        assertEquals("\"true\"^^<" + XSD.BOOLEAN + ">", value.getQueryString());
    }

    @Test
    void createShortValue() {
        final ParameterValue value = sut.create((short) 117);
        assertEquals((short) 117, value.getValue());
        assertEquals("\"117\"^^<" + XSD.SHORT + ">", value.getQueryString());
    }

    @Test
    void createIntegerValue() {
        final ParameterValue value = sut.create(117);
        assertEquals(117, value.getValue());
        assertEquals("\"117\"^^<" + XSD.INT + ">", value.getQueryString());
    }

    @Test
    void createLongValue() {
        final long v = System.currentTimeMillis();
        final ParameterValue value = sut.create(v);
        assertEquals(v, value.getValue());
        assertEquals("\"" + v + "\"^^<" + XSD.LONG + ">", value.getQueryString());
    }

    @Test
    void createFloatValue() {
        final ParameterValue value = sut.create(3.14f);
        assertEquals(3.14f, value.getValue());
        assertEquals("\"3.14\"^^<" + XSD.FLOAT + ">", value.getQueryString());
    }

    @Test
    void createDoubleValue() {
        final ParameterValue value = sut.create(3.14);
        assertEquals(3.14, value.getValue());
        assertEquals("\"3.14\"^^<" + XSD.DOUBLE + ">", value.getQueryString());
    }

    @Test
    void createCreatesTemporalParameterValueFromJavaUtilDate() {
        final Date date = new Date();
        final ParameterValue value = sut.create(date);
        assertEquals(date.toInstant(), value.getValue());
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createUriValue() {
        final URI uri = URI.create("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = sut.create(uri);
        assertEquals(uri, value.getValue());
        assertEquals("<" + uri + ">", value.getQueryString());
    }

    @Test
    void createUrlValue() throws Exception {
        final URL url = new URL("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = sut.create(url);
        assertEquals(url.toURI(), value.getValue());    // URLs are internally transformed to URIs
        assertEquals("<" + url + ">", value.getQueryString());
    }

    @Test
    void createUntypedValue() {
        final Integer integer = 117;
        final ParameterValue value = sut.createUntyped(integer);
        assertEquals(integer, value.getValue());
        assertEquals(integer.toString(), value.getQueryString());
    }

    @Test
    void createValueSupportsCreatingParameterValueFromEntityInstance() {
        final OWLClassA value = Generators.generateOwlClassAInstance();
        when(metamodelProvider.isEntityType(value.getClass())).thenReturn(true);
        final ParameterValue result = sut.create(value);
        assertThat(result, instanceOf(EntityParameterValue.class));
        assertEquals(value, result.getValue());
    }

    @Test
    void createLocalDateTimeValueCreatesTemporalParameter() {
        final LocalDateTime localDateTime = LocalDateTime.now();
        final ParameterValue value = sut.create(localDateTime);
        assertEquals(localDateTime, value.getValue());
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createInstantValueCreatesTemporalParameter() {
        final Instant instant = Instant.now();
        final ParameterValue value = sut.create(instant);
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createZonedDateTimeValueCreatesTemporalParameter() {
        final ZonedDateTime zonedDateTime = ZonedDateTime.now();
        final ParameterValue value = sut.create(zonedDateTime);
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createOffsetDateTimeValueCreatesTemporalParameter() {
        final OffsetDateTime offsetDateTime = OffsetDateTime.now();
        final ParameterValue value = sut.create(offsetDateTime);
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createLocalDateValueCreatesTemporalParameter() {
        final LocalDate localDate = LocalDate.now();
        final ParameterValue value = sut.create(localDate);
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createLocalTimeValueCreatesTemporalParameter() {
        final LocalTime localTime = LocalTime.now();
        final ParameterValue value = sut.create(localTime);
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createOffsetTimeValueCreatesTemporalParameter() {
        final OffsetTime offsetTime = OffsetTime.now();
        final ParameterValue value = sut.create(offsetTime);
        assertThat(value, instanceOf(TemporalParameterValue.class));
    }

    @Test
    void createCollectionValueCreatesCollectionOfUriParametersValuesForUriElements() {
        final Collection<URI> uris = Arrays
                .asList(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        final ParameterValue result = sut.create(uris);
        assertEquals(uris.stream().map(u -> "<" + u + ">").collect(Collectors.joining(",")), result.getQueryString());
    }

    @Test
    void createCollectionValueCreatesCollectionOfLiteralsForLiteralElements() {
        final Collection<Object> values = Arrays.asList(117, false);
        final ParameterValue result = sut.create(values);
        assertEquals("\"" + 117 + "\"^^<" + XSD.INT + ">,\"" + false + "\"^^<" + XSD.BOOLEAN + ">",
                result.getQueryString());
    }

    @Test
    @MockitoSettings(strictness = Strictness.LENIENT)
    void createCollectionValueCreatesCollectionOfUrisForEntityElements() throws Exception {
        final Collection<OWLClassA> values = Arrays
                .asList(Generators.generateOwlClassAInstance(), Generators.generateOwlClassAInstance());
        when(metamodelProvider.isEntityType(any())).thenReturn(false);
        when(metamodelProvider.isEntityType(OWLClassA.class)).thenReturn(true);
        final MetamodelMocks mocks = new MetamodelMocks();
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        mocks.setMocks(metamodel);
        when(metamodelProvider.getMetamodel()).thenReturn(metamodel);
        final ParameterValue result = sut.create(values);
        assertEquals(values.stream().map(a -> "<" + a.getUri() + ">").collect(Collectors.joining(",")),
                result.getQueryString());
    }

    @Test
    void createCreatesDurationParameterValueForJavaDuration() {
        final Duration duration = Duration.ofHours(10);
        final ParameterValue value = sut.create(duration);
        assertThat(value, instanceOf(DurationParameterValue.class));
    }

    @Test
    void createCreatesDurationParameterValueForJavaPeriod() {
        final Period period = Period.ofDays(365);
        final ParameterValue value = sut.create(period);
        assertThat(value, instanceOf(DurationParameterValue.class));
    }

    @Test
    void createCreatesIriParameterValueForIri() {
        final IRI iri = IRI.create("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = sut.create(iri);
        assertThat(value, instanceOf(IriParameterValue.class));
    }
}
