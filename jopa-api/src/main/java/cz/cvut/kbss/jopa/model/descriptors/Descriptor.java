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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.exceptions.AmbiguousContextException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.net.URI;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;

/**
 * Used to specify additional metadata for entities and their attributes.
 * <p>
 * Descriptors are composed in a hierarchical structure representing the object graph they describe.
 */
public interface Descriptor {

    /**
     * Gets contexts for this descriptor.
     * <p>
     * An empty result indicates the default context.
     * <p>
     * Note that for saving, there must be at most one context.
     *
     * @return Context URIs
     */
    Set<URI> getContexts();

    /**
     * Gets the only context specified by this descriptor.
     * <p>
     * This method will check whether there is at most one context specified in this descriptor. If there are none
     * (meaning the default should be used), an empty {@link Optional} is returned. If there are more than one, an
     * {@link AmbiguousContextException} is thrown.
     * <p>
     * This method is intended to be used by data modification operations, which require at most one target context to
     * be specified.
     *
     * @return Single entity context wrapped in {@code Optional}, empty, if there is none
     * @throws AmbiguousContextException If there are more than one context specified
     */
    Optional<URI> getSingleContext();

    /**
     * Adds the specified context to this descriptor.
     * <p>
     * Note that adding the default context removes all the previously added contexts, as they become obsolete.
     *
     * @param context Context to add, {@code null} indicates default context
     * @return This instance
     */
    Descriptor addContext(URI context);

    /**
     * Gets the language set for this descriptor.
     *
     * @return Language tag (e.g. en, cs), can be {@code null}, meaning any language is supported or the language tag
     * has not been set (see {@link #hasLanguage()})
     */
    String getLanguage();

    /**
     * Gets information about whether language tag has been set on this descriptor.
     * <p>
     * The language tag can be explicitly set to {@code null}, meaning any language is supported. This can be used to
     * override PU-level language setting.
     *
     * @return {@code true} if a language tag has been set on this descriptor, {@code false} otherwise
     */
    boolean hasLanguage();

    /**
     * Sets language tag of this descriptor.
     * <p>
     * Applies to any possible sub-descriptors as well.
     *
     * @param languageTag The language tag to use, possibly {@code null}, meaning no language preference should be used
     * @return This instance
     * @see #anyLanguage()
     */
    Descriptor setLanguage(String languageTag);

    /**
     * Configures this descriptor to support any language tag (including no language tags).
     * <p>
     * This is useful for overriding previously set language tag expectations (either on PU level or parent descriptor
     * level).
     * <p>
     * This does the same as calling {@link #setLanguage(String)} with {@code null} argument, but is more explicit.
     *
     * @return This instance
     */
    Descriptor anyLanguage();

    /**
     * Gets attribute descriptors specified in this descriptor.
     *
     * @return Unmodifiable view of attribute descriptors
     */
    Collection<Descriptor> getAttributeDescriptors();

    /**
     * Gets descriptor for the specified attribute.
     *
     * @param attribute Entity attribute, as specified by the application metamodel
     * @return Descriptor
     * @throws IllegalArgumentException If the descriptor is not available
     */
    Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute);

    /**
     * Gets contexts in which the property assertion(s) of the specified attribute are stored.
     * <p>
     * If none are specified for the attribute, contexts of this descriptor are returned.
     * <p>
     * Note that for saving a property assertion, there must be at most one context.
     *
     * @param attribute Entity attribute, as specified by the application model
     * @return Context identifiers
     */
    Set<URI> getAttributeContexts(FieldSpecification<?, ?> attribute);

    /**
     * Gets the only context specified by this descriptor for the specified attribute.
     * <p>
     * If no context is specified (meaning the default context should be used), an empty {@link Optional} is returned.
     * <p>
     * If more than one context are available for the specified attribute, an {@link AmbiguousContextException} is
     * thrown.
     *
     * @param attribute Entity attribute, as specified by the application model
     * @return Context identifier
     * @throws AmbiguousContextException If a unique attribute context cannot be determined
     * @see #getAttributeContexts(FieldSpecification)
     */
    Optional<URI> getSingleAttributeContext(FieldSpecification<?, ?> attribute);

    /**
     * Adds descriptor for the specified attribute.
     * <p>
     * If a descriptor already exists for the specified attribute, it is overridden by the new one.
     *
     * @param attribute  The attribute to set descriptor for
     * @param descriptor The descriptor to use
     * @return This instance
     */
    Descriptor addAttributeDescriptor(FieldSpecification<?, ?> attribute, Descriptor descriptor);

    /**
     * Adds repository context for the specified attribute.
     * <p>
     * This in effect means creating a descriptor (if it does not already exist) for the specified field and adding the
     * specified context to it.
     *
     * @param attribute The attribute to set context for
     * @param context   The context to set
     * @return This instance
     * @see #addAttributeDescriptor(FieldSpecification, Descriptor)
     */
    Descriptor addAttributeContext(FieldSpecification<?, ?> attribute, URI context);

    /**
     * Sets language to be used when working (retrieving, persisting) with values of the specified attribute.
     * <p>
     * Note that setting language in this manner will not have any effect on descriptors of the specified attribute
     * previously retrieved from this descriptor.
     *
     * @param attribute   The attribute concerned
     * @param languageTag Language tag to use, possibly {@code null}
     * @return This instance
     */
    Descriptor setAttributeLanguage(FieldSpecification<?, ?> attribute, String languageTag);

    /**
     * Whether property assertion should be stored in the subject's context (default), or whether they should be stored
     * together with the assertion value.
     * <p>
     * This applies to object references, literal values are always stored in the specified context.
     *
     * @return Whether property assertion is stored in the subject context
     */
    boolean areAssertionsInSubjectContext();

    /**
     * Whether this descriptor overrides assertion context.
     * <p>
     * Assertions are typically stored in the subject context, even if their target is in a different context (see
     * {@link #areAssertionsInSubjectContext()}). However, literal values have to be stored in the assertion context
     * only, so descriptors of fields holding literal values can override the default assertion target context to use
     * the field descriptor context for assertion as well.
     *
     * @return Boolean indicating whether assertion context is based on object descriptor
     * @see #areAssertionsInSubjectContext()
     */
    default boolean overridesAssertionContext() {
        return false;
    }

    /**
     * Whether to include inferred statements when loading data corresponding to this descriptor.
     * <p>
     * Note that this setting is taken into account only for attributes that are declared as possibly containing
     * inferred values (using the {@link cz.cvut.kbss.jopa.model.annotations.Inferred} annotation). For explicit
     * attributes, this setting is ignored.
     * <p>
     * The setting in this descriptor is applied recursively to all the descriptors it may be composed of, unless a
     * different value is specified explicitly for them.
     *
     * @return Whether to include inferred values for inferred attributes
     * @see #disableInference()
     * @see #enableInference()
     */
    boolean includeInferred();

    /**
     * Instructs this descriptor to specify that only asserted statements should be loaded for otherwise inferred
     * attributes.
     *
     * @return This descriptor
     * @see #includeInferred()
     */
    Descriptor disableInference();

    /**
     * Instructs this descriptor to specify that both inferred and asserted statements should be loaded for otherwise
     * inferred attributes.
     * <p>
     * This is the default setting.
     *
     * @return This descriptor
     * @see #includeInferred()
     */
    Descriptor enableInference();
}
