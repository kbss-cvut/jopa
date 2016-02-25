/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver;

public final class OntoDriverProperties {

	private OntoDriverProperties() {
		throw new AssertionError();
	}

	/**
	 * Property for setting default auto-commit strategy for connections.
	 */
	public static final String CONNECTION_AUTO_COMMIT = "cz.cvut.kbss.ontodriver.connection-auto-commit";
	/**
	 * Reasoner factory class property.
	 */
	public static final String OWLAPI_REASONER_FACTORY_CLASS = "cz.cvut.jopa.reasonerFactoryClass";
	/**
	 * Ontology language property.
	 */
	public static final String ONTOLOGY_LANGUAGE = "cz.cvut.jopa.lang";

	/**
	 * This setting tells the driver whether to use the transactional ontology
	 * for retrieving entities and answering queries. </p>
	 * 
	 * If so, uncommitted changes made during transaction will be included in
	 * query evaluation, entity retrieval etc. Otherwise the driver will use the
	 * ontology as it was when the transaction was started and uncommitted
	 * changes will not be visible until commit.
	 */
	public static final String USE_TRANSACTIONAL_ONTOLOGY = "cz.cvut.kbss.ontodriver.use-transactional-onto";

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
	 * Property for specifying extra URIs which should be added to the module
	 * extraction signature. </p>
	 * 
	 * The module extraction signature is generated from metamodel, but
	 * <i>types</i> and <i>properties</i> cannot be determined from the
	 * metamodel. Therefore it is possible to specify them using this property
	 * so that the module is complete.
	 */
	public static final String MODULE_EXTRACTION_SIGNATURE = "cz.cvut.kbss.ontodriver.module-signature";

	/**
	 * Property representing module extraction signature delimiter. </p>
	 * 
	 * I. e. URIs in module extraction signature are delimited by this string.
	 * 
	 * @see #MODULE_EXTRACTION_SIGNATURE
	 */
	public static final String SIGNATURE_DELIMITER = "|";
}
