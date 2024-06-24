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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.gen.GeneratedEntityClass;
import cz.cvut.kbss.jopa.proxy.reference.GeneratedEntityReferenceProxy;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

/**
 * Metamodel-related utility functions.
 */
public class MetamodelUtils {

    private MetamodelUtils() {
        throw new AssertionError();
    }

    /**
     * Checks whether the specified set of types contains any types not contained in the current module extraction
     * signature and if so, it adds them into the signature.
     *
     * @param types     The types to check (can be {@code null})
     * @param metamodel Persistence unit metamodel containing module extraction signature
     */
    public static void checkForModuleSignatureExtension(Collection<?> types, Metamodel metamodel) {
        Objects.requireNonNull(metamodel);
        if (types == null || types.isEmpty()) {
            return;
        }
        final Set<URI> signature = metamodel.getModuleExtractionExtraSignature();
        for (Object elem : types) {
            final URI u = EntityPropertiesUtils.getValueAsURI(elem);
            if (!signature.contains(u)) {
                metamodel.addUriToModuleExtractionSignature(u);
            }
        }
    }

    /**
     * Gets an entity class corresponding to the specified class.
     * <p>
     * This method returns either the provided class or its superclass in case when the provided class is a generated
     * subclass created by JOPA.
     *
     * @param cls Class to process
     * @param <T> Type
     * @return Entity class
     */
    public static <T> Class<? super T> getEntityClass(Class<T> cls) {
        return (cls.getAnnotation(GeneratedEntityClass.class) != null || cls.getAnnotation(GeneratedEntityReferenceProxy.class) != null)
                ? cls.getSuperclass() : cls;
    }
}
