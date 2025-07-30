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

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.ontodriver.model.LangString;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.util.Collection;
import java.util.Date;
import java.util.Objects;
import java.util.stream.Collectors;

public class ParameterValueFactory {

    private final MetamodelProvider metamodelProvider;

    public ParameterValueFactory(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
    }

    /**
     * Returns a new variable parameter specification.
     * <p>
     * This is the default implementation, if a parameter is not set, a variable is used in the query to represent an
     * unbound parameter.
     *
     * @param name Parameter (variable) name
     * @return Parameter value object
     */
    public ParameterValue createVariableValue(String name) {
        return new NamedVariableParameterValue(name);
    }

    /**
     * Returns a new variable parameter specification.
     * <p>
     * This is the default implementation, if a parameter is not set, a variable is used in the query to represent an
     * unbound parameter.
     *
     * @param position Parameter (variable) position
     * @return Parameter value object
     */
    public ParameterValue createVariableValue(Integer position) {
        return new PositionalVariableParameterValue(position);
    }

    /**
     * Returns new String parameter value specification.
     * <p>
     * The language tag is optional.
     *
     * @param value    The value
     * @param language Language tag of the value, e.g. en, cz. Optional
     * @return Parameter value object
     */
    public ParameterValue create(String value, String language) {
        return new StringParameterValue(value, language);
    }

    /**
     * Returns new parameter value specification.
     *
     * @param value The value
     * @return Parameter value object
     */
    public ParameterValue create(Object value) {
        Objects.requireNonNull(value);
        if (value instanceof URI uri) {
            return new UriParameterValue(uri);
        } else if (value instanceof URL url) {
            try {
                return new UriParameterValue(url.toURI());
            } catch (URISyntaxException e) {
                throw new IllegalArgumentException("Unable to transform the specified URL to URI.", e);
            }
        } else if (value instanceof Boolean b) {
            return new BooleanParameterValue(b);
        } else if (value instanceof Short s) {
            return new ShortParameterValue(s);
        } else if (value instanceof Integer i) {
            return new IntegerParameterValue(i);
        } else if (value instanceof Long lng) {
            return new LongParameterValue(lng);
        } else if (value instanceof Double dbl) {
            return new DoubleParameterValue(dbl);
        } else if (value instanceof Float fl) {
            return new FloatParameterValue(fl);
        } else if (value instanceof TemporalAccessor tmpAccessor) {
            return new TemporalParameterValue(tmpAccessor);
        } else if (value instanceof Date date) {
            return new TemporalParameterValue(date.toInstant());
        } else if (value instanceof TemporalAmount temporalAmount) {
            return new DurationParameterValue(temporalAmount);
        } else if (metamodelProvider.isEntityType(value.getClass())) {
            return new EntityParameterValue(value, metamodelProvider);
        } else if (value instanceof LangString langString) {
            return new StringParameterValue(langString);
        } else if (value instanceof Collection<?> col) {
            return new CollectionParameterValue(col.stream().map(this::create).collect(Collectors.toList()));
        } else if (value instanceof IRI iri) {
            return new IriParameterValue(iri);
        }else {
            return new StringParameterValue(value.toString());
        }
    }

    /**
     * Returns new untyped parameter value specification.
     *
     * @param value The value
     * @return Parameter value object
     */
    public ParameterValue createUntyped(Object value) {
        return new UntypedParameterValue(Objects.requireNonNull(value));
    }
}
