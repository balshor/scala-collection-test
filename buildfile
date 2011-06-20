require 'buildr/scala'

repositories.remote << "http://www.ibiblio.org/maven2/"

define 'scala-collection-test' do
  project.version = '0.1'
  test.using :specs
  package :jar
end
