/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.CacheManager;

import java.net.URI;
import java.util.Set;

public class DisabledCacheManager implements CacheManager {

    @Override
    public void add(Object identifier, Object entity, Descriptor descriptor) {
        // Do nothing
    }

    @Override
    public <T> T get(Class<T> cls, Object identifier, Descriptor descriptor) {
        return null;
    }

    @Override
    public void evictInferredObjects() {
        // Do nothing
    }

    @Override
    public void setInferredClasses(Set<Class<?>> inferredClasses) {
        // Do nothing
    }

    @Override
    public void close() {
        // Do nothing
    }

    @Override
    public boolean contains(Class<?> cls, Object identifier, Descriptor descriptor) {
        return false;
    }

    @Override
    public void evict(Class<?> cls, Object identifier, URI context) {
        // Do nothing
    }

    @Override
    public void evict(Class<?> cls) {
        // Do nothing
    }

    @Override
    public void evict(URI contextUri) {
        // Do nothing
    }

    @Override
    public void evictAll() {
        // Do nothing
    }
}
