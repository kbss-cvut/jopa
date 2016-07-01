/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.config;

public abstract class SesameOntoDriverProperties {

    private SesameOntoDriverProperties() {
        throw new AssertionError();
    }

    /**
     * Specifies whether a in-memory storage should be used for local Sesame
     * repositories. </p>
     *
     * When set to true, any local Sesame repositories that are created by the
     * driver are created as only MemoryStores without any persistent backend.
     * Repositories accessed over the Internet or already existing locally are
     * not affected by this setting. </p>
     *
     * {@code Boolean} value expected, default is false.
     */
    public static final String SESAME_USE_VOLATILE_STORAGE = "cz.cvut.kbss.ontodriver.sesame.use-volatile-storage";

    /**
     * Specifies whether Sesame inference (RDFS, forward chaining) should be
     * used. </p>
     *
     * Note that this setting applies only to local storages (in memory or
     * native), remote storages use their own inference settings. </p>
     *
     * {@code Boolean} value expected, default is false.
     */
    public static final String SESAME_USE_INFERENCE = "cz.cvut.kbss.ontodriver.sesame.use-inference";

    /**
     * Specifies how many requested assertions suffice to perform load all.
     *
     * More specifically, if the number of assertions requested by an {@link cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor}
     * is low enough, the statements for them will be loaded by asking Sesame for statements with subject and property bound.
     *
     * Otherwise, statements will be loaded using only subject bound and will be filtered according to the assertions. This
     * will in most cases have better performance than loading with bound property.
     */
    public static final String SESAME_LOAD_ALL_THRESHOLD = "cz.cvut.kbss.ontodriver.sesame.load-all-threshold";
}
