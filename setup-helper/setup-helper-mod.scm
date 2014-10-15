;;;; setup-helper-mod.scm -*- Hen -*-
;;;; Kon Lovett, Apr '12

(module setup-helper-mod

	(;export
		extension-name-and-version
		extension-name
		extension-version
		verify-extension-name
		CHICKEN-SOURCE-EXTENSION
		CHICKEN-IMPORT-EXTENSION
		CHICKEN-INLINE-EXTENSION
		CHICKEN-TYPES-EXTENSION
		HTML-EXTENSION
		STATIC-ARCHIVE-EXTENSION
		OBJECT-BINARY-EXTENSION
		EXECUTABLE-EXTENSION
		DIRECTORY-SEPARATOR
		installation-chicken-home
		installation-repository-path
		directory-separator?
		filename
		make-directory-name
		document-filename
		source-filename
		shared-library-filename
		shared-filename
		static-library-filename
		static-filename
		import-filename
		source-import-filename
		shared-import-filename
		inline-filename
		types-filename
		program-filename
		make-home-pathname
		make-repository-pathname
		srfi-29-bundles-home
		make-srfi-29-bundle-directory-name
		copy-file-relative
		copy-file-absolute
		copy-to-repository
		copy-to-home
		compile-static
		compile-shared
		compile-static-module
		compile-shared-module
		install-in-repository
		install-in-home
		install-srfi-29-bundle
		install-static-extension
		install-shared-extension
		install-static-extension-module
		install-shared-extension-module
		install-shared+static-extension-module
		install-extension-tag
		setup-static-extension
		setup-shared-extension
		setup-static-extension-module
		setup-shared-extension-module
		setup-shared+static-extension-module
		default-static-compile-options
		default-shared-compile-options
		default-import-compile-options
		default-static-install-options
		default-shared-install-options
		default-static-module-install-options
		default-shared-module-install-options
		default-shared+static-module-install-options )

	(import scheme chicken)

	(include "setup-helper")

) ;module setup-helper-mod
