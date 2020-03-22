/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.net.URL;
import java.time.*;
import java.util.Date;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

/**
 * Datatype specifications in the assertions are taken from the SPARQL specification.
 */
class ParameterValueFactoryTest {

    @Mock
    private MetamodelProvider metamodelProvider;

    @InjectMocks
    private ParameterValueFactory sut;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
    }

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
        assertEquals("\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>", value.getQueryString());
    }

    @Test
    void createShortValue() {
        final ParameterValue value = sut.create((short) 117);
        assertEquals((short) 117, value.getValue());
        assertEquals("\"117\"^^<http://www.w3.org/2001/XMLSchema#short>", value.getQueryString());
    }

    @Test
    void createIntegerValue() {
        final ParameterValue value = sut.create(117);
        assertEquals(117, value.getValue());
        assertEquals("\"117\"^^<http://www.w3.org/2001/XMLSchema#int>", value.getQueryString());
    }

    @Test
    void createLongValue() {
        final long v = System.currentTimeMillis();
        final ParameterValue value = sut.create(v);
        assertEquals(v, value.getValue());
        assertEquals("\"" + v + "\"^^<http://www.w3.org/2001/XMLSchema#long>", value.getQueryString());
    }

    @Test
    void createFloatValue() {
        final ParameterValue value = sut.create(3.14f);
        assertEquals(3.14f, value.getValue());
        assertEquals("\"3.14\"^^<http://www.w3.org/2001/XMLSchema#float>", value.getQueryString());
    }

    @Test
    void createDoubleValue() {
        final ParameterValue value = sut.create(3.14);
        assertEquals(3.14, value.getValue());
        assertEquals("\"3.14\"^^<http://www.w3.org/2001/XMLSchema#double>", value.getQueryString());
    }

    @Test
    void createDateValue() {
        final Date date = new Date();
        final ParameterValue value = sut.create(date);
        assertEquals(date, value.getValue());
        assertEquals("\"" + date.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                value.getQueryString());
    }

    @Test
    void createUriValue() {
        final URI uri = URI.create("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = sut.create(uri);
        assertEquals(uri, value.getValue());
        assertEquals("<" + uri.toString() + ">", value.getQueryString());
    }

    @Test
    void createUrlValue() throws Exception {
        final URL url = new URL("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = sut.create(url);
        assertEquals(url.toURI(), value.getValue());    // URLs are internally transformed to URIs
        assertEquals("<" + url.toString() + ">", value.getQueryString());
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
    void createLocalDateTimeValueCreatesDateTimeParameter() {
        final LocalDateTime localDateTime = LocalDateTime.now();
        final ParameterValue value = sut.create(localDateTime);
        assertEquals("\"" + localDateTime.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                value.getQueryString());
    }

    @Test
    void createInstantValueCreatesDateTimeParameter() {
        final Instant instant = Instant.now();
        final ParameterValue value = sut.create(instant);
        assertEquals("\"" + instant.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                value.getQueryString());
    }

    @Test
    void createZonedDateTimeValueCreatesDateTimeParameter() {
        final ZonedDateTime zonedDateTime = ZonedDateTime.now();
        final ParameterValue value = sut.create(zonedDateTime);
        assertEquals(
                "\"" + zonedDateTime.toOffsetDateTime().toString() + "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                value.getQueryString());
    }

    @Test
    void createOffsetDateTimeValueCreatesDateTimeParameter() {
        final OffsetDateTime offsetDateTime = OffsetDateTime.now();
        final ParameterValue value = sut.create(offsetDateTime);
        assertEquals(
                "\"" + offsetDateTime.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                value.getQueryString());
    }

    @Test
    void createLocalDateValueCreatesDateParameter() {
        final LocalDate localDate = LocalDate.now();
        final ParameterValue value = sut.create(localDate);
        assertEquals("\"" + localDate.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#date>",
                value.getQueryString());
    }

    @Test
    void createLocalTimeValueCreatesTimeParameter() {
        final LocalTime localTime = LocalTime.now();
        final ParameterValue value = sut.create(localTime);
        assertEquals("\"" + localTime.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#time>",
                value.getQueryString());
    }

    @Test
    void createOfssetTimeValueCreatesTimeParameter() {
        final OffsetTime offsetTime = OffsetTime.now();
        final ParameterValue value = sut.create(offsetTime);
        assertEquals("\"" + offsetTime.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#time>",
                value.getQueryString());
    }
}
