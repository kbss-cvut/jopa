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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.exceptions.AmbiguousContextException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * Defines base descriptor, which is used to specify context information for entities and their fields.
 * <p>
 * The descriptor hierarchy is a classical <b>Composite</b> pattern.
 */
public abstract class AbstractDescriptor implements Descriptor {

    protected final Set<URI> contexts = new HashSet<>(4);

    protected final boolean assertionsInSubjectContext;

    private String language;
    private boolean hasLanguage;

    private boolean includeInferred = true;

    protected AbstractDescriptor() {
        this(true);
    }

    protected AbstractDescriptor(boolean assertionsInSubjectContext) {
        this.assertionsInSubjectContext = assertionsInSubjectContext;
    }

    protected AbstractDescriptor(URI context) {
        this(context, true);
    }

    protected AbstractDescriptor(URI context, boolean assertionsInSubjectContext) {
        if (context != null) {
            contexts.add(context);
        }
        this.assertionsInSubjectContext = assertionsInSubjectContext;
    }

    protected AbstractDescriptor(Set<URI> contexts, boolean assertionsInSubjectContext, String language,
                                 boolean hasLanguage,
                                 boolean includeInferred) {
        this.assertionsInSubjectContext = assertionsInSubjectContext;
        this.language = language;
        this.hasLanguage = hasLanguage;
        this.includeInferred = includeInferred;
        this.contexts.addAll(contexts);
    }

    @Override
    public Set<URI> getContexts() {
        return Collections.unmodifiableSet(contexts);
    }

    @Override
    public Optional<URI> getSingleContext() {
        return retrieveSingleContext(contexts);
    }

    private static Optional<URI> retrieveSingleContext(Set<URI> col) {
        if (col.size() > 1) {
            throw new AmbiguousContextException("Expected at most one context, but got " + col);
        }
        return col.isEmpty() ? Optional.empty() : Optional.of(col.iterator().next());
    }

    @Override
    public Descriptor addContext(URI context) {
        if (context == null) {
            contexts.clear();
        } else {
            contexts.add(context);
        }
        return this;
    }

    @Override
    public Optional<URI> getSingleAttributeContext(FieldSpecification<?, ?> attribute) {
        return retrieveSingleContext(getAttributeContexts(attribute));
    }

    @Override
    public String getLanguage() {
        return language;
    }

    @Override
    public boolean hasLanguage() {
        return hasLanguage;
    }

    @Override
    public Descriptor setLanguage(String languageTag) {
        this.language = languageTag;
        this.hasLanguage = true;
        return this;
    }

    @Override
    public Descriptor anyLanguage() {
        return setLanguage(null);
    }

    @Override
    public boolean areAssertionsInSubjectContext() {
        return assertionsInSubjectContext;
    }

    @Override
    public boolean includeInferred() {
        return includeInferred;
    }

    @Override
    public Descriptor disableInference() {
        this.includeInferred = false;
        return this;
    }

    @Override
    public Descriptor enableInference() {
        this.includeInferred = true;
        return this;
    }

    protected void setIncludeInferred(boolean includeInferred) {
        this.includeInferred = includeInferred;
    }

    @Override
    public boolean equals(Object o) {
        return equalsImpl(o);
    }

    protected boolean equalsImpl(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || !getClass().equals(o.getClass())) {
            return false;
        }
        final AbstractDescriptor that = (AbstractDescriptor) o;
        if (hasLanguage != that.hasLanguage) {
            return false;
        }
        if (assertionsInSubjectContext != that.assertionsInSubjectContext) {
            return false;
        }
        return Objects.equals(contexts, that.contexts) && Objects.equals(language, that.language);
    }

    protected boolean equals(Object other, Map<VisitedPair, Boolean> visited) {
        return equalsImpl(other);
    }

    @Override
    public int hashCode() {
        return hashCodeImpl();
    }

    protected int hashCodeImpl() {
        int result = contexts.hashCode();
        result = 31 * result + (language != null ? language.hashCode() : 0);
        result = 31 * result + (hasLanguage ? 1 : 0);
        result = 31 * result + (assertionsInSubjectContext ? 1 : 0);
        return result;
    }

    protected int hashCode(Map<Object, Boolean> visited) {
        return hashCodeImpl();
    }

    @Override
    public String toString() {
        return contexts.isEmpty() ? "default_context" : contexts.toString();
    }

    protected static class VisitedPair {
        final Object a, b;

        VisitedPair(Object a, Object b) {
            this.a = a;
            this.b = b;
        }

        public boolean equals(Object o) {
            if (!(o instanceof VisitedPair p)) {return false;}
            return a == p.a && b == p.b;
        }

        public int hashCode() {
            return System.identityHashCode(a) * 31 + System.identityHashCode(b);
        }
    }
}
