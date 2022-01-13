/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Wraps a {@link MultilingualString} so that calls to modifying operations are intercepted and reported to the
 * persistence context (if necessary).
 */
public class IndirectMultilingualString extends MultilingualString implements IndirectWrapper {

    private final Object owner;
    private final Field field;
    private final UnitOfWorkImpl persistenceContext;

    private final MultilingualString referencedString;

    protected IndirectMultilingualString() {
        this.owner = null;
        this.field = null;
        this.persistenceContext = null;
        this.referencedString = new MultilingualString();
    }

    /**
     * Create new indirect multilingual string backed by the specified referenced {@link MultilingualString}.
     *
     * @param owner            Owner of the string
     * @param f                The field holding this string
     * @param uow              Persistence context the owner belongs to
     * @param referencedString The string to reference
     * @throws NullPointerException If the {@code referencedString} is null
     */
    public IndirectMultilingualString(Object owner, Field f, UnitOfWorkImpl uow, MultilingualString referencedString) {
        this.owner = owner;
        this.field = f;
        this.persistenceContext = Objects.requireNonNull(uow);
        this.referencedString = Objects.requireNonNull(referencedString);
    }

    private void notifyPersistenceContext() {
        assert persistenceContext != null;
        if (persistenceContext.isInTransaction() && !persistenceContext.isInCommit()) {
            persistenceContext.attributeChanged(owner, field);
        }
    }

    @Override
    public MultilingualString set(String language, String value) {
        referencedString.set(language, value);
        notifyPersistenceContext();
        return this;
    }

    @Override
    public MultilingualString set(String value) {
        referencedString.set(value);
        notifyPersistenceContext();
        return this;
    }

    @Override
    public String get(String language) {
        return referencedString.get(language);
    }

    @Override
    public String get() {
        return referencedString.get();
    }

    @Override
    public boolean contains(String language) {
        return referencedString.contains(language);
    }

    @Override
    public boolean containsSimple() {
        return referencedString.containsSimple();
    }

    @Override
    public boolean isEmpty() {
        return referencedString.isEmpty();
    }

    @Override
    public void remove(String language) {
        referencedString.remove(language);
        notifyPersistenceContext();
    }

    @Override
    public Set<String> getLanguages() {
        return referencedString.getLanguages();
    }

    @Override
    public Map<String, String> getValue() {
        return referencedString.getValue();
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof MultilingualString) {
            if (o instanceof IndirectMultilingualString) {
                return referencedString.equals(((IndirectMultilingualString) o).referencedString);
            }
            return referencedString.equals(o);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return referencedString.hashCode();
    }

    @Override
    public String toString() {
        return referencedString.toString();
    }

    @Override
    public MultilingualString unwrap() {
        return referencedString;
    }
}
