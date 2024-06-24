/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.datatype.xsd;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

/**
 * Provides a shared instance of {@link javax.xml.datatype.DatatypeFactory}, so that individual mappers do not need to manage their own.
 * <p>
 * Note that this assumes the factory implementation is thread-safe.
 */
class DatatypeFactoryProvider {

    private static final DatatypeFactory FACTORY = initFactory();

    private DatatypeFactoryProvider() {
        throw new AssertionError();
    }

    private static DatatypeFactory initFactory() {
        try {
            return DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new IllegalStateException("Unable to initialize javax.xml.datatype.DatatypeFactory.", e);
        }
    }

    static DatatypeFactory getFactory() {
        return FACTORY;
    }
}
