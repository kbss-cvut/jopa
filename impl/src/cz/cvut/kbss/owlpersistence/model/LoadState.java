package cz.cvut.kbss.owlpersistence.model;

public enum LoadState {

	/**
	 * The state of the element is known to have been loaded.
	 */
	LOADED,

	/**
	 * The state of the element is known not to have been loaded.
	 */
	NOT_LOADED,

	/**
	 * The state of the element cannot be determined.
	 */
	UNKNOWN
}
