/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.exceptions.AmbiguousContextException;

import java.net.URI;
import java.util.*;

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

    @Override
    public Set<URI> getContexts() {
        return Collections.unmodifiableSet(contexts);
    }

    @Override
    public Optional<URI> getSingleContext() {
        if (contexts.size() > 1) {
            throw new AmbiguousContextException("Expected at most one context, but got " + contexts);
        }
        return contexts.isEmpty() ? Optional.empty() : Optional.of(contexts.iterator().next());
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
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AbstractDescriptor)) {
            return false;
        }

        AbstractDescriptor that = (AbstractDescriptor) o;

        if (hasLanguage != that.hasLanguage) {
            return false;
        }
        if (assertionsInSubjectContext != that.assertionsInSubjectContext) {
            return false;
        }
        return Objects.equals(contexts, that.contexts) && Objects.equals(language, that.language);
    }

    @Override
    public int hashCode() {
        int result = contexts.hashCode();
        result = 31 * result + (language != null ? language.hashCode() : 0);
        result = 31 * result + (hasLanguage ? 1 : 0);
        result = 31 * result + (assertionsInSubjectContext ? 1 : 0);
        return result;
    }

    @Override
    public String toString() {
        return contexts.isEmpty() ? "default_context" : contexts.toString();
    }
}
