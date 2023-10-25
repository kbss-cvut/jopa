/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.utils.NamespaceResolver;

/**
 * Context holding data when an {@link IdentifiableType} is being built.
 *
 * @param <X> Java type represented by the type
 */
class TypeBuilderContext<X> {

    private final AbstractIdentifiableType<X> type;

    private final NamespaceResolver namespaceResolver;

    private ConverterResolver converterResolver;

    private String puLanguage;

    TypeBuilderContext(AbstractIdentifiableType<X> type, NamespaceResolver namespaceResolver) {
        this.type = type;
        this.namespaceResolver = namespaceResolver;
    }

    void registerNamespace(String prefix, String namespace) {
        namespaceResolver.registerNamespace(prefix, namespace);
    }

    String resolveNamespace(String iri) {
        return namespaceResolver.resolveFullIri(iri);
    }

    AbstractIdentifiableType<X> getType() {
        return type;
    }

    ConverterResolver getConverterResolver() {
        return converterResolver;
    }

    void setConverterResolver(ConverterResolver converterResolver) {
        this.converterResolver = converterResolver;
    }

    String getPuLanguage() {
        return puLanguage;
    }

    void setPuLanguage(String puLanguage) {
        this.puLanguage = puLanguage;
    }
}
