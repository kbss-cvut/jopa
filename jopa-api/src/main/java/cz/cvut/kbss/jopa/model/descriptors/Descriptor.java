/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Defines base descriptor, which is used to specify context information for entities and their fields.
 * <p>
 * The descriptor hierarchy is a classical <b>Composite</b> pattern.
 *
 * @author ledvima1
 */
public abstract class Descriptor {

    protected final URI context;

    protected Descriptor() {
        this(null);
    }

    protected Descriptor(URI context) {
        this.context = context;
    }

    /**
     * Gets context for this descriptor.
     * <p>
     * Note that the context URI may be {@code null}, meaning that the default context is referenced
     *
     * @return Context URI
     */
    public URI getContext() {
        return context;
    }

    /**
     * Gets attribute descriptors specified in this descriptor.
     *
     * @return Unmodifiable view of attribute descriptors
     */
    public abstract Collection<Descriptor> getAttributeDescriptors();

    /**
     * Gets descriptor for the specified attribute.
     *
     * @param attribute Entity attribute, as specified by the application metamodel
     * @return Descriptor
     * @throws IllegalArgumentException If the descriptor is not available
     */
    public abstract Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute);

    /**
     * Adds descriptor for the specified attribute.
     *
     * @param attribute  The attribute to set descriptor for
     * @param descriptor The descriptor to use
     */
    public abstract void addAttributeDescriptor(Field attribute, Descriptor descriptor);

    /**
     * Adds repository context for the specified attribute.
     * <p>
     * This in effect means creating a descriptor for the specified field with the specified context.
     *
     * @param attribute The attribute to set context for
     * @param context   The context to set
     * @see #addAttributeDescriptor(Field, Descriptor)
     */
    public abstract void addAttributeContext(Field attribute, URI context);

    /**
     * Gets all contexts present in this descriptor.
     * <p>
     * If any of the descriptors specifies the default context, an empty set is returned.
     * <p>
     * In case of entity descriptor this means recursively asking all of its attributes for their context.
     *
     * @return Set of context URIs or an empty set, if the default one should be used
     */
    public Set<URI> getAllContexts() {
        Set<URI> contexts = new HashSet<>();
        contexts = getContextsInternal(contexts, new HashSet<>());
        return contexts != null ? contexts : Collections.<URI>emptySet();
    }

    /**
     * Gets the contexts, discarding any already visited descriptors.
     *
     * @param contexts Contexts collected so far
     * @param visited  Visited descriptors
     * @return Already visited contexts + those found in this descriptor
     */
    protected abstract Set<URI> getContextsInternal(Set<URI> contexts, Set<Descriptor> visited);

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((context == null) ? 0 : context.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Descriptor other = (Descriptor) obj;
        if (context == null) {
            if (other.context != null)
                return false;
        } else if (!context.equals(other.context))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return context != null ? context.toString() : "default_context";
    }
}
