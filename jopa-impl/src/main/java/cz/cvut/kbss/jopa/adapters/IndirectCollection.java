package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.lang.reflect.Field;

public abstract class IndirectCollection<T> {

    protected final Object owner;
    protected final Field field;
    protected final UnitOfWorkImpl persistenceContext;

    protected IndirectCollection() {
        owner = null;
        field = null;
        persistenceContext = null;
    }

    /**
     * Create new indirect collection from the specified data. </p>
     * <p>
     * The owner can be null, the persistence context not.
     *
     * @param owner              Owner of the indirect collection
     * @param f                  The field holding this collection
     * @param persistenceContext Persistence context the owner belongs to
     * @throws NullPointerException If the persistence context is null
     */
    protected IndirectCollection(Object owner, Field f, UnitOfWorkImpl persistenceContext) {
        if (persistenceContext == null) {
            throw new NullPointerException("Null passed in as persistenceContext.");
        }
        this.owner = owner;
        this.field = f;
        this.persistenceContext = persistenceContext;
    }

    protected void persistChange() {
        if (persistenceContext.isInTransaction() && !persistenceContext.isInCommit()) {
            persistenceContext.attributeChanged(owner, field);
        }
    }

    /**
     * The returned type is determined by the instance type parameter.
     *
     * @return The collection wrapped in this indirect collection
     */
    public abstract T getReferencedCollection();
}
