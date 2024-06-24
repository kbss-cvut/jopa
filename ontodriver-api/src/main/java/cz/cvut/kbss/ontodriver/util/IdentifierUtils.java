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
package cz.cvut.kbss.ontodriver.util;

import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.net.URL;
import java.util.Objects;
import java.util.Random;
import java.util.Set;

/**
 * Utility for working with resource identifiers.
 */
public class IdentifierUtils {

    public static final Set<Class<?>> IDENTIFIER_TYPES = Set.of(NamedResource.class, URI.class, URL.class);

    private static final Random RANDOM = new Random();

    private IdentifierUtils() {
        throw new AssertionError();
    }

    /**
     * Generates a (pseudo) random identifier based on the specified class URI.
     * <p>
     * The identifier consists of the class URI and then contains the string 'instance' and a random integer to ensure
     * uniqueness. The 'instance' part is appended after a slash or a _, if the class URI contains a hash fragment.
     *
     * @param classUri Class URI used as identifier base
     * @return Generated identifier
     */
    public static URI generateIdentifier(URI classUri) {
        Objects.requireNonNull(classUri);
        if (classUri.getFragment() != null) {
            return URI.create(classUri + "_instance" + RANDOM.nextInt());
        } else {
            String base = classUri.toString();
            if (base.endsWith("/")) {
                return URI.create(base + "instance" + RANDOM.nextInt());
            } else {
                return URI.create(base + "/instance" + RANDOM.nextInt());
            }
        }
    }

    /**
     * Checks if the specified class represents a resource identifier.
     *
     * @param cls Class to check
     * @return {@code true} if instances of the specified class represent resource identifiers, {@code false} otherwise
     * @see #IDENTIFIER_TYPES
     */
    public static boolean isResourceIdentifierType(Class<?> cls) {
        return IDENTIFIER_TYPES.contains(cls);
    }
}
