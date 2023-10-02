mod api;
pub(crate) mod model;

use std::net::SocketAddr;

use axum::Router;
use tower_http::cors::CorsLayer;

use crate::api::routes_api;

#[tokio::main]
async fn main() {
    let cors = CorsLayer::permissive();

    let routes = Router::new().nest("/api", routes_api()).layer(cors);

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    axum::Server::bind(&addr)
        .serve(routes.into_make_service())
        .await
        .unwrap();
}
