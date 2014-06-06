window.onload = function() {
    var listOfBuildsTemplate = document.getElementById("listOfBuildsTemplate").innerHTML;
    var buildTemplate = document.getElementById("buildTemplate").innerHTML;

    var app = angular.module("app", ["ngRoute"]);

    app.config(function($routeProvider) {
        $routeProvider.when("/", {
            controller: "IndexCtrl",
            template: listOfBuildsTemplate
        });

        $routeProvider.when("/:projectName", {
            controller: "IndexCtrl",
            template: listOfBuildsTemplate
        });

        $routeProvider.otherwise({
            redirectTo: "/"
        });
    });

    app.controller("IndexCtrl", function($scope, $interval, $routeParams, ResultService) {
        var projectName = $routeParams.projectName;
        $scope.results = [];

        $scope.reloadBuilds = function() {
            ResultService.getResults(projectName)
                .then(function(data) {
                    $scope.results = data.results;
                });
        };

        var reloadHandle = $interval($scope.reloadBuilds, 3000);
        $scope.$on("$destroy", function() {
            $interval.cancel(reloadHandle);
        });

        $scope.reloadBuilds();
    });

    app.service("ResultService", function($http, $q) {
        this.getResults = function(projectName) {
            var deferred = $q.defer();
            var url = document.location.href.split("#")[0]
                + (projectName ? projectName + "/" : "")
                + "results.json";

            $http({method: "GET", url: url})
                .success(function(data) {
                    deferred.resolve(data);
                }).error(function(data) {
                    console.log(data);
                    deferred.reject();
                });

            return deferred.promise;
        };
    });

    app.directive("showBuild", function() {
        return {
            restrict: "E",
            template: buildTemplate,
            scope: { build: "=" },
            replace: true,
            link: function(scope, element, attrs) {
                scope.showShortOutput = true;
                scope.shortOutput     = scope.build.output.substr(0, 200) + " ..."

                scope.toggleView = function () {
                    scope.showShortOutput = !scope.showShortOutput;
                };
            }
        };
    });

    angular.bootstrap(document, ["app"]);
};
